package slaq.util

final case class BatchResult(
  generatedKeys: List[Long] = List.empty,
  affectedRows: Option[Int] = None
)
