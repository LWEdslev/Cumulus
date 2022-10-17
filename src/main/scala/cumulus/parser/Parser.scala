package cumulus.parser

import cumulus.AST.*
import cumulus.parser.Tokens.*
import cumulus.parser.Lexer.*

import scala.annotation.tailrec
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

object Parser extends PackratParsers {

  def parseTokens(tokens: Seq[CuToken]): Exp =
    prog(CuTokenReader(tokens)) match
      case p @ (NoSuccess(_, _) | Failure(_, _) | Error(_, _)) => throw SyntaxError(p.next.pos)
      case Success(result, _) => result

  def parse(code: String): Exp =
    parseTokens(Lexer.apply(code))

  override type Elem = CuToken

  private lazy val prog: PackratParser[Exp] = phrase { expr() }

  private def expr(antiprecedence: Int = 2): PackratParser[Exp] =
    antiprecedence match
      case x if x >= 0 =>
        binopexp(antiprecedence) |
        expr(x - 1)
      case -1 =>
        expr(-2)
      case -2 =>
        literal |
        parens

  private lazy val parens: PackratParser[Exp] =
    (LEFT_PAREN() ~ expr() ~ RIGHT_PAREN()) ^^ { case _ ~ exp ~ _ => exp }

  private lazy val literal: PackratParser[Lit] =
    intlit ^^ { lit => IntLit(lit.i) }

  private lazy val intlit: PackratParser[INT] =
    accept("int literal", { case lit: INT => lit})

  private def binopexp(antiprecedence: Int): PackratParser[Exp] =
    expr(antiprecedence - 1) * {
      binop(antiprecedence) ^^ { op => { (left: Exp, right: Exp) => BinOpExp(left, op, right).setPos(left.pos) }}
    }

  private def binop(antiPrecedence: Int): PackratParser[BinOp] = positioned {
    antiPrecedence match {
      case 0 =>
        mult | div | modulo
      case 1 =>
        plus | minus
      case 2 =>
        max
    }
  }

  private lazy val plus: PackratParser[BinOp] = OP("+") ^^ { _ => PlusBinOp() }

  private lazy val minus: PackratParser[BinOp] = OP("-") ^^ { _ => MinusBinOp() }

  private lazy val mult: PackratParser[BinOp] = OP("*") ^^ { _ => MultBinOp() }

  private lazy val div: PackratParser[BinOp] = OP("/") ^^ { _ => DivBinOp() }

  private lazy val max: PackratParser[BinOp] = OP("max") ^^ { _ => MaxBinOp() }

  private lazy val modulo: PackratParser[BinOp] = OP("%") ^^ { _ => ModuloBinOp() }

  private lazy val neg: PackratParser[UnOp] = OP("-") ^^ { _ => NegUnOp() }

  private lazy val identifier: PackratParser[IDENTIFIER] =
    accept("identifier", { case id: IDENTIFIER => id })

  class CuTokenReader(tokens: Seq[CuToken]) extends Reader[CuToken] {
    override def first: CuToken = tokens.head
    override def atEnd: Boolean = tokens.isEmpty
    override def pos: Position = tokens.headOption.map(_.pos).getOrElse(NoPosition)
    override def rest: Reader[CuToken] = CuTokenReader(tokens.tail)
  }
}