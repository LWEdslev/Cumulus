package cumulus

import cumulus.Parser.Lexer._

object Main {
  def main(args: Array[String]): Unit = {
    val code = "=>match case class .new"
    val tokens = apply(code)
    println(tokens)
  }
}
