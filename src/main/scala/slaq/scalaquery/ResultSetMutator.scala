package slaq

trait ResultSetMutator[T] {

  /**
   * Get the current row's value.
   */
  def row: T

  /**
   * Update the current row.
   */
  def row_=(value: T): Unit

  /**
   * Insert a new row.
   */
  def insert(value: T): Unit

  /**
   * Delete the current row.
   */
  def delete(): Unit
}
