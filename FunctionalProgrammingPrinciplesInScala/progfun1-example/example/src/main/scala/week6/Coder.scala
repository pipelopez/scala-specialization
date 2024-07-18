package week6

/**
 * cuando en los teléfonos cada número representaba unas letras del abcdario
 * */

class Coder(words: List[String]):
  val mnemonics = Map(
    '2' -> "ABC",
    '3' -> "DEF",
    '4' -> "GHI",
    '5' -> "JKL",
    '6' -> "MNO",
    '7' -> "PQRS",
    '8' -> "TUV",
    '9' -> "WXYZ"
  )

  /**
   * pasa una letra al dígito que representa
   * */
  private val charCode : Map[Char, Char] =
    for (digit, str) <- mnemonics; ltr <- str yield ltr -> digit


  /**
   * pasa una palabra al string de dígitos que la representa
   * */
  private def wordCode(word: String): String = word.toUpperCase.map(charCode)

  /**
   * pasa un string de dígitos a todas las palabras en el diccionarios que la representan
   * */
  private val wordsForNum: Map[String, List[String]] = words.groupBy(wordCode).withDefaultValue(Nil)

  /**
   * todas las formas de codificar un numero como una lista de palabras
   * */
  def encode(number: String): Set[List[String]] =
    if number.isEmpty then Set(Nil)
    else
      for{
        splitPoint <- (1 to number.length).toSet
        word <- wordsForNum(number.take(splitPoint))
        rest <- encode(number.drop(splitPoint))
      } yield word :: rest

val coder = Coder(List(
    "Scala", "Python", "Ruby", "C", "rocks", "socks", "sucks", "works", "pack"
  )).encode("7225276257").map(_.mkString(" "))

@main def testCoder = println(s"$coder")


