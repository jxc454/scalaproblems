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

  def runLengthEncoding[T](list: List[T]): List[(T, Int)] = (list :\ (Nil: List[(T, Int)]))((k, acc) => acc match {
    case Nil if list == Nil => Nil
    case Nil => (k, 1) :: Nil
    case h :: t if k == h._1 => (k, h._2 + 1) :: t
    case _ => (k, 1) :: acc
  })

  def runLengthEncoding2[T](list: List[T]): List[Either[T, (T, Int)]] = runLengthEncoding(list)
    .map(tup => if (tup._2 == 1) Left(tup._1) else Right(tup))

  def runLengthEncodingWithSpan[T](list: List[T]): List[(T, Int)] = list match {
    case Nil => Nil
    case _ =>
      val (front: List[T], back: List[T]) = list.span(_ == list.head)
      (list.head, front.length) :: runLengthEncodingWithSpan(back)
  }

  def decodeRLE[T](rle: List[(T, Int)]): List[T] = rle.flatMap(k => List.fill(k._2)(k._1))

  def dupListElements[T](l: List[T]): List[T] = l.flatMap(k => List(k, k))

  def dupListElementsN[T](l: List[T], n: Int): List[T] = l.flatMap(k => List.fill(n)(k))

  def dropEveryNth[T](l: List[T], n: Int): List[T] = l.zip(List.fill(1 + l.length / n)(1 to n).flatten).filterNot(_._2 == n).map(_._1)

  def dropEveryNth2[T](l: List[T], n: Int): List[T] = {
    val (init: List[List[T]], last: List[List[T]]) = l.grouped(n).toList.splitAt(l.length / n)
    init.flatMap(_.init) ++ last.flatten.take(n - 1)
  }

  def rotateN[T](l: List[T], n: Int): List[T] = {
    val (first: List[T], last: List[T]) = l.splitAt(n)
    last ++ first
  }

  def nQueens(n: Int): List[List[(Int, Int)]] = {
    def isSafe(queens: List[(Int, Int)], queen: (Int, Int)): Boolean = {
      if (queens.map(_._1).contains(queen._1) ||
        queens.map(_._2).contains(queen._2) ||
        queens.map(rc => rc._1 - rc._2).contains(queen._1 - queen._2) ||
        queens.map(rc => rc._1 + rc._2).contains(queen._1 + queen._2)
      ) false else true
    }

    def placeQueens(row: Int): List[List[(Int, Int)]] = {
      if (row == 0) return List(Nil)
      if (row == 1) return (1 to n).map(column => List((row, column))).toList

      (1 to n)
        .flatMap(column => placeQueens(row - 1).map(next => (row, column) :: next))
        .filter(k => isSafe(k.tail, k.head))
        .toList
    }

    placeQueens(n)
  }
}
