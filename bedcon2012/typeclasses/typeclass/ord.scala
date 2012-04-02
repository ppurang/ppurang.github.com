//https://gist.github.com/b4f5a72b19411d8b6d92

import Predef.{implicitly => ?}

type BooleanCompare[A] = A => A => Boolean

trait Ord[A] {
  def max : A => A => A
  def min : A => A => A
  def < : BooleanCompare[A]
  def > : BooleanCompare[A]
  def <= : BooleanCompare[A]
  def >= : BooleanCompare[A]
}

implicit object IntOrd extends Ord[Int] {
  def max = a => b => if (a > b) a else b
  def min = a => b => if (a > b) b else a
  def < = a => b => a < b
  def > = a => b => a > b
  def <= = a => b => a <= b
  def >= = a => b => a >= b
}

//implicits
def quickSortDescendingOrder[A](l: List[A])(implicit x: Ord[A]): List[A] = l match {
  case p :: xs => quickSortDescendingOrder(xs filter (y => x.<(p)(y))) ::: p :: quickSortDescendingOrder(xs filter (y => x.>=(p)(y)))
  case _ => Nil
}

//context bounds
def quickSortAscendingOrder[A: Ord](l: List[A]): List[A] = l match {
  case p :: xs => quickSortAscendingOrder(xs filter (y => ?[Ord[A]].>=(p)(y))) ::: p :: quickSortAscendingOrder(xs filter (y => ?[Ord[A]].<(p)(y)))
  case _ => Nil
}

val tl = List(1,2,3,4,5,99,89,3,4,5,6,100,1000,39)

quickSortDescendingOrder(tl)

quickSortAscendingOrder(tl)

implicit object IntTupleOrd extends Ord[(Int, Int)] {
  def max = a => b => if (a._1 > b._1 && a._2 > b._2) a else b
  def min = a => b => if (a._1 > b._1 && a._2 > b._2) b else a

  def < = a => b => a._1 < b._1 && a._2 < b._2
  
  def > = a => b => a._1 > b._1 && a._2 > b._2
  
  def <= = a => b => a._1 <= b._1 && a._2 <= b._2
  
  def >= = a => b => a._1 >= b._1 && a._2 >= b._2
}

quickSort2(List((3, 5), (2, 4), (3, 4)))
