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

  def flatten[T](l: List[List[T]]): List[T] = l match {
    case Nil => Nil
    case h :: t => h ::: flatten(t)
  }

  def removeConsecutiveDupes(str: String): String = {
    def removeIfMatch(str: String, strMatch: Char): String = {
      if (str == "") {
        ""
      } else if (str.startsWith(strMatch.toString)) {
        removeIfMatch(str.tail, strMatch)
      } else {
        str.head + removeIfMatch(str.tail, str.head)
      }
    }

    removeIfMatch(str=str, strMatch='\0')
  }

  def removeConsecutiveDupes[T](list: List[T]): List[T] = (list :\ (Nil: List[T]))((k, acc) => acc match {
    case Nil => k :: Nil
    case h :: _ if k == h => acc
    case _ => k :: acc
  })

  def consecutiveSubLists[T](list: List[T]): List[List[T]] = (list :\ (Nil: List[List[T]]))((k, acc) => acc match {
    case Nil if list == Nil => Nil
    case Nil => List(k :: Nil)
    case h :: t if k == h.head => (k :: h) :: t
    case _ => (k :: Nil) :: acc
  })

}
