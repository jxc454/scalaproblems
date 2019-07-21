package jcc

import org.scalatest._

class ForCompsTest extends FunSpec with MustMatchers {
  val books: Seq[Book] = ForComps.books
  books.length must be(100)
}
