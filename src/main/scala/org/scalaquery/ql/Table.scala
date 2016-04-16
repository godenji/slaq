package org.scalaquery.ql

import org.scalaquery.Fail
import org.scalaquery.session.{PositionedResult, PositionedParameters}
import org.scalaquery.util.{Node, UnaryNode}
import core.{Profile, QueryTemplate, ColumnOptions}

abstract class Table[T](
	val schemaName: Option[String], 
	val tableName: String, 
	private var maybeJoin: Option[Join]) extends ColumnBase[T] {
	
	def this(_tableName: String) = this(None, _tableName, None)
	
	def tableJoin = maybeJoin
	override def isNamedTable = true
  def nodeChildren = Nil
  override def toString = s"Table $tableName"
  
  type ProfileType = Profile
  val O: ColumnOptions = ColumnOptions
  
  def * : ColumnBase[T]
  final def column[C : TypeMapper]
		(name: String, options: ColumnOption[C, ProfileType]*) = {
		
			val node = Node(this) match {
				// if node delegate is a join then store join ref in target tables
				case j @ Join(
					t1 @ Table.Alias(left: Table[_]), 
					t2 @ Table.Alias(right: Table[_]), _, _) =>
						
					List(left, right).foreach(_.maybeJoin = Some(j))
					Node(
						if(left.tableName == tableName) t1 else t2
					)
				// if node delegate is not a join then unset existing join ref
				case ta @ Table.Alias(t: Table[_]) if t.tableJoin.isDefined =>
					t.maybeJoin = None
					ta
				case x => x
			}
  		new NamedColumn[C](node, name, options:_*)
	}
	
	def createFinderBy[P](f: this.type => NamedColumn[P])
  	(implicit profile: Profile, tm: TypeMapper[P]): QueryTemplate[P,T] = {
  	
    Params[P](tm).flatMap{p=> 
    	Query(this).filter{case(t: Table.this.type)=> 
    		ColumnOps.Is(f(t), p)
    	}
    }(profile)
  }
  
	def create_* : Iterable[NamedColumn[_]] = {
		def createTableError(msg: Option[String]) = Fail(
			s"Cannot use ${msg.getOrElse("")} in ${tableName}.* CREATE TABLE statement"
		)
  	def f(n:Node): Iterable[NamedColumn[_]] = n match {
      case p:Projection[_] =>
        0 until p.productArity map (n => Node(p.productElement(n)) match {
          case c: NamedColumn[_] => c
          case c => createTableError(Some(s"column $c"))
        })
      case n:NamedColumn[_] => Iterable(n)
      case _ => createTableError(None)
    }
    f(Node(*))
  }

  def foreignKey[P, PU, TT <: Table[_], U]
    (name: String, sourceColumns: P, targetTable: TT)
    (
    	targetColumns: TT => P, 
    	onUpdate: ForeignKeyAction = ForeignKeyAction.NoAction,
      onDelete: ForeignKeyAction = ForeignKeyAction.NoAction
    )
    (implicit unpackT: Unpack[TT, U], unpackP: Unpack[P, PU]): 
    
    ForeignKeyQuery[TT, U] = {
	    val targetUnpackable = Unpackable(
	    	targetTable.mapOp(Table.Alias), unpackT
	    )
	    val fk = new ForeignKey(
	    	name, this, targetUnpackable, targetTable, unpackP,
	      sourceColumns, targetColumns, onUpdate, onDelete
	    )
	    new ForeignKeyQuery(List(fk), targetUnpackable)
  }

  def primaryKey[TT](name: String, sourceColumns: TT)
  	(implicit unpack: Unpack[TT, _]): PrimaryKey = 
  		PrimaryKey(name, unpack.linearizer(sourceColumns).getLinearizedNodes)

  def tableConstraints: Iterator[Constraint] = 
  	for {
      m <- getClass().getMethods.iterator
      if m.getParameterTypes.length == 0 && 
      	classOf[Constraint].isAssignableFrom(m.getReturnType)
      q = m.invoke(this).asInstanceOf[Constraint]
    } yield q

  final def foreignKeys: Iterable[ForeignKey[_ <: Table[_], _]] =
    tableConstraints.collect{
    	case q: ForeignKeyQuery[_,_] => q.fks }.toIndexedSeq.flatten

  final def primaryKeys: Iterable[PrimaryKey] =
    tableConstraints.collect{ case k: PrimaryKey => k }.toIndexedSeq

  def index[TT](name: String, on: TT, unique: Boolean = false)
  	(implicit unpack: Unpack[TT, _]) = new Index(
  		name, this, unpack.linearizer(on).getLinearizedNodes, unique
  	)

  def indexes: Iterable[Index] = (for {
      m <- getClass().getMethods.view
      if m.getReturnType == classOf[Index] && m.getParameterTypes.length == 0
    } yield m.invoke(this).asInstanceOf[Index])

  def getLinearizedNodes = *.getLinearizedNodes
  
  def getResult(profile:Profile, rs: PositionedResult) = 
  	*.getResult(profile, rs)
  	
  def updateResult(profile:Profile, rs: PositionedResult, value: T) =
  	*.updateResult(profile, rs, value)
  	
  def setParameter(profile:Profile, ps: PositionedParameters, value: Option[T]) = 
  	*.setParameter(profile, ps, value)
	
	def ddl(implicit profile: ProfileType): DDL = profile.buildTableDDL(this)
}

object Table {
  def unapply[T](t: Table[T]) = Some(t.tableName)

  case class Alias(child: Node) extends UnaryNode {
    override def toString = s"Table.Alias $child"
    override def isNamedTable = true
  }
  case class Ref(table: Node, tableJoin: Option[Join])
  case class Name(alias: String, isFresh: Boolean)
}
