package cumulus

import cumulus.AST.{BinOpExp, IntLit, PlusBinOp}
import cumulus.parser.Lexer._
import cumulus.parser.Parser.parse
import cumulus.Interpreter._

object Main {
  def main(args: Array[String]): Unit = {
    val code =
      """
          {let hello = "Hello"; let space = " "+3.1415926 ; let world = "world"+1 ; hello + space + world}
      """
    val tokens = apply(code)
    println(tokens)
    val ast = parse(code)
    println(ast)
    val out = evalPretty(code)
    println(out)
  }
}
