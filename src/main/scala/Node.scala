import scala.collection.mutable

abstract class Node() {
  var parent: Node = null
  val children = mutable.MutableList[Node]()

  def +=(n: Node): Node = {
    n.parent = this
    children += n
    n
  }

  def run(mem: Memory)
}

class RootNode extends Node {
  override def run(mem: Memory) = children.foreach(_.run(mem))
}

class WhileNode extends Node {
  override def run(mem: Memory) {
    while (mem.value != 0) {
      children.foreach(_.run(mem))
    }
  }
}

class PtrIncrNode extends Node {
  override def run(mem: Memory) = mem.incr()
}

class PtrDecrNode extends Node {
  override def run(mem: Memory) = mem.decr()
}

class ValIncrNode extends Node {
  override def run(mem: Memory) = mem.incrVal()
}

class ValDecrNode extends Node {
  override def run(mem: Memory) = mem.decrVal()
}

class PutNode extends Node {
  override def run(mem: Memory) = print(mem.value.toChar)
}

class GetNode extends Node {
  override def run(mem: Memory) = mem.value(System.in.read())
}