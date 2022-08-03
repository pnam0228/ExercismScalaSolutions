class Cipher(val key: String) {
  private val numAlphabet = 'z' - 'a' + 1

  def encode(origStr: String): String = {
    val encodedChars = origStr.zip(key)
      .map { case (s, k) => (s + k - 2 * 'a') % numAlphabet + 'a' }
      .map(_.toChar)
      .toArray
    String.valueOf(encodedChars)
  }

  def decode(encoded: String): String = {
    val decodedChars = encoded.zip(key)
      .map { case (s, k) => (s - k + numAlphabet) % numAlphabet + 'a' }
      .map{_.toChar}
      .toArray
    String.valueOf(decodedChars)
  }
}

object Cipher {
  def apply(keyOpt: Option[String]): Cipher = keyOpt match {
    case Some(value) =>
      val trimmedValue = value.trim
      if (trimmedValue.nonEmpty && trimmedValue.forall(c => c.isLetter && c.isLower)) new Cipher(value)
      else throw new IllegalArgumentException()
    case None => new Cipher(String.valueOf(Array.fill(100)('a')))
  }
}