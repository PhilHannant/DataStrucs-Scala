abstract class BST

case class Node(left: BST, value: Int, right: BST) extends BST

case class Empty() extends BST

object BST {

  def insert(v: Int, tree: BST): BST = {
    tree match {
      case Node(left, value, right) =>
        if (v > value) Node(left, value, insert(v, right))
        else if (v < value) Node(insert(v, left), value, right)
        else tree
      case Empty() => Node(Empty(), v, Empty())
    }
  }

  def find(x: Int, tree: BST): Boolean = {
    tree match {
      case Empty() => false
      case (Node(l, v, r)) =>
        if (x == v) true
        else if (x < v) find(x, l)
        else find(x, r)
    }
  }

  def printInorder(tree: BST): Unit = {
    tree match {
      case Node(l, v, r) =>
        printInorder(l)
        println(v)
        printInorder(r)
      case Node(Empty(), v, Empty()) => print(v)
      case Empty() => print(" ")
    }
  }
}

object Tester extends App {
  var t = BST.insert(3, Empty())
  t = BST.insert(10, t)
  t = BST.insert(72, t)
  t = BST.insert(21, t)
  t = BST.insert(18, t)
  t = BST.insert(4, t)
  t = BST.insert(3, t)
  t = BST.insert(1, t)

  BST.printInorder(t)
  println(BST.find(1, t))

}


