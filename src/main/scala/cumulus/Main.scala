package cumulus

import cumulus.Parser.Lexer._

object Main {
  def main(args: Array[String]): Unit = {
    val code = "//"
    println(code)
    val tokens = apply(code)
    println(tokens)
  }
}
