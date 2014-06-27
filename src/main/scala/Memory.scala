import scala.collection.mutable.ArrayBuffer

class Memory {
  private var pointer = 0
  private val cells = new ArrayBuffer[Int]

  cells.append(0)

  def ptr = pointer

  def value = cells(pointer)
  def value(v: Int) = cells(pointer) = v

  def incrVal() = cells(pointer) += 1
  def decrVal() = cells(pointer) -= 1

  def incr() = {
    pointer += 1
    if (pointer >= cells.length) {
      cells.append(0)
    }
  }

  def decr() = {
    pointer -= 1
    if (cells.last == 0) {
      cells.trimEnd(1)
    }
  }
}
