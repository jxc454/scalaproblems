package jcc

import scala.annotation.tailrec

object Problems {
  @tailrec
  def last[T](l: List[T]): Option[T] = l match {
    case Nil => None
    case h :: Nil => Option(h)
    case _ :: t => last(t)
  }

  @tailrec
  def penultimate[T](l: List[T]): Option[T] = l match {
    case Nil => None
    case _ :: Nil => None
    case h :: _ :: Nil => Some(h)
    case _ :: t => penultimate(t)
  }

  @tailrec
  def atIndex[T](l: List[T], index: Int): Option[T] = l match {
    case Nil => None
    case h :: _ if index == 0 => Some(h)
    case _ :: t if index > 0 => atIndex(t, index - 1)
    case _ if index < 0 => None
  }

  def length[T](l: List[T]): Int = (0 /: l)((acc, _) => acc + 1)

  def reverse[T](l: List[T]): List[T] = ((Nil: List[T]) /: l)((acc, k) => k :: acc)

  def isPalindrome[T](l: List[T]): Boolean = l == Problems.reverse(l)
  def isPalindrome2[T](l: List[T]): Boolean = (0 until l.length / 2).forall(index => l(index) == l(l.length - 1 - index))

  @tailrec
  def isPalindrome3[T](l: List[T]): Boolean = l match {
    case Nil => true
    case _ :: Nil => true
    case h :: t =>
      val init :+ last = t
      h == last && isPalindrome3(init)
  }
}
