package jcc

import org.joda.time.{DateTime, DateTimeZone}
import scala.util.Random

object ForComps {
  val size: Int = 100
  val r: Random = new Random

  def randomString(n: Int): String = ((Nil: List[Char]) /: (1 to n))((acc, _) =>
    r.nextPrintableChar() :: acc).mkString("")

  def randomDate: DateTime = new DateTime(
    1950 + r.nextInt(25),
    r.nextInt(11) + 1,
    r.nextInt(27) + 1, 0, 0, 0,
    DateTimeZone.UTC
  )

  val books: Seq[Book] = ((Nil: List[Book]) /: (1 to size))((acc, _) =>
    Book((0 to r.nextInt(3)).map(_ => Author(randomString(2), randomDate)),
      randomString((r.nextGaussian() + 12).toInt),
      (r.nextGaussian() * 15 + 2000).toInt) :: acc)

  books.foreach(k => println(k.toString))
}

case class Book(authors: Seq[Author], title: String, year: Int)
case class Author(name: String, dob: DateTime)

