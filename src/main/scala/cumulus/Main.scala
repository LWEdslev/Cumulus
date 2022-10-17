package cumulus

import cumulus.AST.{BinOpExp, IntLit, PlusBinOp}
import cumulus.parser.Lexer.*
import cumulus.parser.Parser.parse

object Main {
  def main(args: Array[String]): Unit = {
    val code = "1+2"
    val tokens = apply(code)
    println(tokens)
    val ast = parse(code)
    println(ast)
  }
}
