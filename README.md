Slaq is a port of Stephan Zeiger's excellent ScalaQuery, a type-safe database query 
API for Scala, similar to FRM (Functional Relational Mapper) libraries like Slick and 
Quill.

Changes to-date:
* added support for Scala 2.13
* added support for multiple table (outer) joins, mirroring sql join syntax
* automatic mapping of user defined value classes to underlying primitve type
* take/drop queries are now cacheable
* map/flatMap `Parameters`call has been renamed to `Params`
* threadLocalSession has been removed
* deprecated `!=` conditional operator has been removed
* =~, =!, &, and | are now available as alternatives to ===, =!=, &&, and ||
* MS Access and Derby support have been removed
* myriad refactorings that improve performance compared to original implementation

It includes the following features:
- Session management based on JDBC Connections
- Type-safe queries based on a query monad and combinators
- Simple static and dynamic queries

The following database systems are directly supported for type-safe queries:
- PostgreSQL
- MySQL
- Microsoft SQL Server
- H2
- HSQLDB/HyperSQL
- Derby/JavaDB
- SQLite
Accessing other database systems is possible, with a reduced feature set.

Licensing conditions (BSD-style) can be found in LICENSE.txt.
