package jcc

object ForComps {
  def getAuthors(books: List[Book]): List[Author] = books.flatMap(_.authors)
  def getAuthorsFC(books: List[Book]): List[Author] = for {book <- books; author <- book.authors} yield author

  def getBooksWithAuthorCount(books: List[Book], authors: Int): Int = books.count(_.authors.length == authors)
  def getBooksWithAuthorCountFC(books: List[Book], authors: Int): Int =
    (for {book <- books if book.authors.length == authors} yield 1).length
}
