package cumulus

import cumulus.AST.{BinOpExp, IntLit, PlusBinOp}
import cumulus.parser.Lexer._
import cumulus.parser.Parser.parse
import cumulus.Interpreter._

object Main {
  def main(args: Array[String]): Unit = {
    val code =
      """
            {fun f(x) = 10*x; f(2)}
      """
    val tokens = apply(code)
    println(tokens)
    val ast = parse(code)
    println(ast)
    val out = evalNew(code)
    println(out)
  }
}
