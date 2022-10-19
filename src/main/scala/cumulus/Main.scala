package cumulus

import cumulus.AST.{BinOpExp, IntLit, PlusBinOp}
import cumulus.parser.Lexer._
import cumulus.parser.Parser.parse
import cumulus.Interpreter._

object Main {
  def main(args: Array[String]): Unit = {
    val code =
      """
          {let x = 1 ; loop if(x < 10) {x = x + 1}; x}
      """
    val tokens = apply(code)
    println(tokens)
    val ast = parse(code)
    println(ast)
    val out = evalNew(code)
    println(out)
  }
}
