package cumulus

import cumulus.AST.{BinOpExp, IntLit, PlusBinOp}
import cumulus.parser.Lexer._
import cumulus.parser.Parser.parse
import cumulus.Interpreter._

object Main {
  def main(args: Array[String]): Unit = {
    val code = "_1//2//3//4//5//6//7_*7*6*5*4*3*2"
    val tokens = apply(code)
    println(tokens)
    val ast = parse(code)
    println(ast)
    val out = eval(ast)
    println(out)
  }
}
