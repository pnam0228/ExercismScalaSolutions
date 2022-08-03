import scala.annotation.tailrec

object Zipper {
  // A zipper for a binary tree.
  // ??? Zipper[A] ???

  // Get a zipper focussed on the root node.
  def fromTree[A](bt: BinTree[A]): Zipper[A] = Zipper(bt, List.empty[Path[A]])

  // Get the complete tree from a zipper.
  @tailrec
  def toTree[A](zipper: Zipper[A]): BinTree[A] = zipper.paths match {
    case _ :: _ => toTree(up(zipper).get)
    case _ => zipper.bt
  }

  // Get the value of the focus node.
  def value[A](zipper: Zipper[A]): A = zipper.bt.value

  // Get the left child of the focus node, if any.
  def left[A](zipper: Zipper[A]): Option[Zipper[A]] = {
    val bt = zipper.bt
    bt.left.map(subTree => Zipper(subTree, Path(Left, bt.value, bt.right) :: zipper.paths))
  }

  // Get the right child of the focus node, if any.
  def right[A](zipper: Zipper[A]): Option[Zipper[A]] = {
    val bt = zipper.bt
    bt.right.map(subTree => Zipper(subTree, Path(Right, bt.value, bt.left) :: zipper.paths))
  }

  // Get the parent of the focus node, if any.
  def up[A](zipper: Zipper[A]): Option[Zipper[A]] = {
    zipper.paths.headOption.map {
      case Path(Left, parentValue, rightSubTree) => Zipper(BinTree(parentValue, Some(zipper.bt), rightSubTree), zipper.paths.tail)
      case Path(Right, parentValue, leftSubTree) => Zipper(BinTree(parentValue, leftSubTree, Some(zipper.bt)), zipper.paths.tail)
    }
  }

  // Set the value of the focus node.
  def setValue[A](v: A, zipper: Zipper[A]): Zipper[A] = {
    val bt = zipper.bt
    Zipper(BinTree(v, bt.left, bt.right), zipper.paths)
  }

  // Replace a left child tree.
  def setLeft[A](l: Option[BinTree[A]], zipper: Zipper[A]): Zipper[A] = {
    val bt = zipper.bt
    Zipper(BinTree(bt.value, l, bt.right), zipper.paths)
  }

  // Replace a right child tree.
  def setRight[A](r: Option[BinTree[A]], zipper: Zipper[A]): Zipper[A] = {
    val bt = zipper.bt
    Zipper(BinTree(bt.value, bt.left, r), zipper.paths)
  }
}

sealed trait Direction
case object Left extends Direction
case object Right extends Direction

case class Path[A](direction: Direction, parentValue: A, otherSubTree: Option[BinTree[A]])

case class Zipper[A](bt: BinTree[A], paths: List[Path[A]])
// A binary tree.
case class BinTree[A](value: A, left: Option[BinTree[A]], right: Option[BinTree[A]])
