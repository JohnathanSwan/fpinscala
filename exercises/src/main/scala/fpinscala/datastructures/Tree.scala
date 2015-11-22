package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {











  // ex 3.25
  def size[A](t:Tree[A]):Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }










  // ex 3.26
  def maximum(t:Tree[Int]):Int = t match {
    case Leaf(x) => x
    case Branch(left, right) => maximum(left) max maximum(right)
  }












  // ex 3.27
  def depth[A](t:Tree[A]):Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }










  // ex 3.28
  def map[A,B](t:Tree[A])(f:A => B):Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(left,right) => Branch(map(left)(f),map(right)(f))
  }











  // ex 3.29
  def fold[A,B](t:Tree[A])(l:A=>B)(b:(B,B)=>B):B = t match {
    case Leaf(a) => l(a)
    case Branch(left,right) => b(fold(left)(l)(b), fold(right)(l)(b))
  }
}
