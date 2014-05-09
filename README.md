This is a port of Stephan Zeiger's excellent ScalaQuery, built against Scala
2.10.4+

Changes to-date:
* added support for multiple table (outer) joins
* take/drop queries are now cacheable
* map/flatMap `Parameters` call has been renamed to `Params`
* threadLocalSession has been removed
* deprecated `!=` conditional operator has been removed
* =~, =!, &, and | are now available as alternatives to ===, =!=, &&, and ||
* MS Access support has been removed

ScalaQuery is a type-safe database query API for Scala.

It includes the following features:
- Session management based on JDBC Connections
- Type-safe queries based on a query monad and combinators
- Simple static and dynamic queries

The following database systems are directly supported for type-safe queries:
- PostgreSQL
- MySQL
- Microsoft SQL Server
~~- Microsoft Access~~ (removed)
- H2
- HSQLDB/HyperSQL
- Derby/JavaDB
- SQLite
Accessing other database systems is possible, with a reduced feature set.

See <http://scalaquery.org/> for more information.
Licensing conditions (BSD-style) can be found in LICENSE.txt.
