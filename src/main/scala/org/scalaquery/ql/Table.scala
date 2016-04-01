package org.scalaquery.ql

import org.scalaquery.SQueryException
import org.scalaquery.ql.core.{Profile,QueryTemplate,Driver,ColumnOptions}
import org.scalaquery.session.{PositionedResult, PositionedParameters}
import org.scalaquery.util.{Node, UnaryNode, WithOp}
import scala.annotation.unchecked.{uncheckedVariance=> uV}

sealed trait TableBase[T] extends Node with WithOp {
  override def isNamedTable = true
}

abstract class Table[T](
	val schemaName: Option[String], 
	val tableName: String) extends TableBase[T] with ColumnBase[T] {
	
	def this(_tableName: String) = this(None, _tableName)
  def nodeChildren = Nil
  override def toString = s"Table $tableName"
  
  type ProfileType = Profile
  val O: ColumnOptions = ColumnOptions
  
  def * : ColumnBase[T]
  def column[C : TypeMapper]
		(n: String, options: ColumnOption[C, ProfileType]*) = 
  		new NamedColumn[C](Node(this), n, options:_*)
	
	final def join[P <: Table[_], U]
		(other: P, joinType: JoinType = JoinType.Inner) = { 
			new JoinBase[this.type, P, T, U](this, other, joinType)
	}
	def leftJoin[P <: Table[_], U](other: P) = join(other, JoinType.Left)
	def rightJoin[P <: Table[_], U](other: P) = join(other, JoinType.Right)
	def outerJoin[P <: Table[_], U](other: P) = join(other, JoinType.Outer)
  
	def createFinderBy[P](f: this.type => NamedColumn[P])
  	(implicit profile:Profile, tm: TypeMapper[P]): QueryTemplate[P,T] = {
  	
    import profile.Implicit._
    Params[P](tm).flatMap{p=> 
    	Query(this).filter{case(t: Table.this.type)=> 
    		ColumnOps.Is(f(t), p)
    	}
    }(profile)
  }
  
	def create_* : Iterable[NamedColumn[_]] = {
		def createTableError(msg: Option[String]) = throw new SQueryException(
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

  final case class Alias(child: Node) extends UnaryNode {
    override def toString = "Table.Alias"
    override def isNamedTable = true
  }
}

class JoinBase[+T1 <: Table[_], +T2 <: Table[_], U1, U2]
	(left: T1, right: T2, joinType: JoinType) {
			
  def on[T <: Column[_] : Queryable]
  	(pred: (T1, T2) => T): Join[T1,T2] = new Join(
  		left, right, joinType, Node( pred(left, right) )
  	)
	/**
	 * foreign key based ON clause<br /><br />
	 * example: 'A join B $ (_.fkey)'
	 * where fkey is B table's foreign key to A
	 */
	def $(fn: T2 => ForeignKeyQuery[T1 @uV,U1]): Join[T1,T2] = {
		val fk = fn(right).fks.head
		new Join(left, right, joinType, ColumnOps.Is(
				fk.left, fk.targetColumnsForOriginalTargetTable
			)
		)
	}
}

final case class Join[+T1 <: Table[_], +T2 <: Table[_]](
	lt: T1, rt: T2, 
	val joinType: JoinType, 
	val on: Node
) extends TableBase[Nothing] {

	def left  = lt.mapOp{n=> JoinPart(Node(n), Node(this))}
  def right = rt.mapOp{n=> JoinPart(Node(n), Node(this))}
  
  def leftNode = Node(lt)
  def rightNode = Node(rt)
  
  def nodeChildren = leftNode :: rightNode :: Nil
  override def toString = s"Join(${Node(lt)}, ${Node(rt)})"
}

object ^ { // Join table extractor; usage: a^b <- A join B on(..)
	def unapply[T1 <: Table[_], T2 <: Table[_]]
		(j: Join[T1, T2]): Option[(T1,T2)] = Some( (j.left,j.right) )
}
object <| { // Join table extractor; usage: a^b <- A join B on(..)
	def unapply[T1 <: Table[_], T2 <: Table[_]]
		(j: Join[T1, T2]): Option[(T1,T2)] = Some( (j.left,j.right) )
}
