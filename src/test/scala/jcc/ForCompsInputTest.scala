package jcc

import org.scalatest._

class ForCompsInputTest extends FunSpec with MustMatchers {
  val books: List[Book] = ForCompsInput.books.toList
  books.length must be(100)

  ForComps.getAuthors(books) must be(ForComps.getAuthorsFC(books))
  ForComps.getBooksWithAuthorCount(books, 2) must be(ForComps.getBooksWithAuthorCountFC(books, 2))
}
