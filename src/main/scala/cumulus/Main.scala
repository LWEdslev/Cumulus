package cumulus

import cumulus.AST.{BinOpExp, IntLit, PlusBinOp}
import cumulus.parser.Lexer._
import cumulus.parser.Parser.parse
import cumulus.Interpreter._

object Main {
  def main(args: Array[String]): Unit = {
    val code = Files.readFile(args.head)
    val out = evalPretty(code)
    println(out)
  }
}
