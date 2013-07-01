package at.iem.point.er.sketches

trait Cell[A] {
  def apply(): A
  def update(value: A): Unit
}