import scala.annotation.tailrec

object Isogram {
  def isIsogram(str: String): Boolean = {
    @tailrec
    def rec(s: String, seenChars: Set[Char]): Boolean = {
      s.isEmpty ||
      !(isLetter(s.head) && seenChars(s.head.toLower)) &&
      rec(s.tail, seenChars + s.head.toLower)
    }

    rec(str, Set.empty[Char])
  }

  def isLetter(c: Char): Boolean = c != ' ' && c != '-'
}