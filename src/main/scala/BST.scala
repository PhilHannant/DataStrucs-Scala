abstract class BST

case class Node(left: BST, value: Int, right: BST) extends BST

case class Empty() extends BST

object BST {

  def insert(v: Int, tree: BST): BST = {
    tree match {
      case Empty() => Node(Empty(), v, Empty())
      case Node(left, value, right) =>
        if(v > value) Node(left, value, insert(v, right))
        else if (v < value) Node(insert(v, left), value, right)
        else tree
    }
  }

  def find(x: Int, tree: BST): Boolean = {
    tree match {
      case Empty() => false
      case (Node(l, v, r)) =>
        if(x == v) true
        else if (x < v) find(x, l)
        else find(x, r)
    }
  }

}