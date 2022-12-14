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
        unopexp |
          simplify |
          expr(-2)
      case -2 =>
        literal |
          ifelseexp |
          funexp |
          identifier ^^ { id => VarExp(id.str).setPos(id.pos) } |
          unitblock |
          block |
          parens

  private lazy val block: PackratParser[Exp] =
    (LEFT_BRACE() ~ blockelmseq ~ expr() ~ RIGHT_BRACE())
      ^^ { case _ ~ d ~ e ~ _ => BlockExp(d, e)}

  private lazy val unitblock: PackratParser[Exp] =
    (LEFT_BRACE() ~ blockelmseq ~ blockel ~ RIGHT_BRACE()) ^^ { case _ ~ ds ~ d ~ _ => BlockExp(ds :+ d, UnitExp()) }

  private lazy val blockel: PackratParser[Decl] = let | fundecl | assign | loopifexp | loopexp

  private lazy val blockelmseq: PackratParser[List[Decl]] = rep { blockel ~ SEMICOLON() } ^^ (_.map(_._1))

  private lazy val fundecl: PackratParser[Decl] =
    (FUN() ~ identifier ~ LEFT_PAREN() ~ identifier ~ paramseq ~ RIGHT_PAREN() ~ EQ() ~ expr())
     ^^ { case _ ~ f ~ _ ~ firstParam ~ params ~ _ ~ _ ~ exp => FunDecl(f.str, List(firstParam.str) ++ params, exp)}

  private def paramseq: PackratParser[List[Var]] = rep { COMMA() ~ identifier } ^^ (_.map(_._2.str))

  private lazy val funexp: PackratParser[Exp] =
    (identifier ~ LEFT_PAREN() ~ expr() ~ RIGHT_PAREN())
     ^^ { case id ~ _ ~  exp ~ _ => FunExp(id.str, List(exp))}

  private lazy val parens: PackratParser[Exp] =
    (LEFT_PAREN() ~ expr() ~ RIGHT_PAREN()) ^^ { case _ ~ exp ~ _ => exp }

  private lazy val simplify: PackratParser[Exp] =
    (OP("_") ~ expr() ~ OP("_")) ^^ { case _ ~ exp ~ _ => UnOpExp(SimplifyOp(), exp)}

  private lazy val listelseq: PackratParser[List[Exp]] = rep { COMMA() ~ expr() } ^^ {_.map(_._2)}

  private lazy val listlit: PackratParser[Exp] =
    (LEFT_BRACKET() ~ expr() ~ listelseq ~ RIGHT_BRACKET())
      ^^ { case _ ~ exp ~ explist ~ _ => ListLit(List(exp) ++ explist) }

  private lazy val emptylist: PackratParser[Exp] =
    (LEFT_BRACKET() ~ RIGHT_BRACKET()) ^^ { case _ ~ _ => ListLit(List()) }

  private lazy val literal: PackratParser[Exp] =
    floatlit ^^ { lit => FloatLit(lit.v) } |
    intlit ^^ { lit => IntLit(lit.i) } |
    boolLit ^^ { lit => BoolLit(lit.b)} |
    stringlit ^^ { lit => StringLit(lit.str)} |
      listlit | emptylist
  private lazy val intlit: PackratParser[INT] =
    accept("int literal", { case lit: INT => lit})

  private lazy val floatlit: PackratParser[FLOAT] =
    accept("float literal", { case lit: FLOAT => lit })

  private lazy val boolLit: PackratParser[BOOL] =
    accept("bool literal", { case lit: BOOL => lit })

  private lazy val stringlit: PackratParser[STRING] =
    accept("string literal", { case lit: STRING => lit })

  private lazy val let: PackratParser[Decl] =
    (LET() ~ identifier ~ EQ() ~ expr()) ^^ { case _ ~ id ~ _ ~ exp => VarDecl(id.str, exp)}

  private lazy val assign: PackratParser[Decl] =
    (identifier ~ EQ() ~ expr()) ^^ { case id ~ _ ~ exp => AssignExp(id.str, exp)}

  private def binopexp(antiprecedence: Int): PackratParser[Exp] =
    expr(antiprecedence - 1) * {
      binop(antiprecedence) ^^ { op => { (left: Exp, right: Exp) => BinOpExp(left, op, right).setPos(left.pos) }}
    }

  private def binop(antiPrecedence: Int): PackratParser[BinOp] = positioned {
    antiPrecedence match {
      case 0 =>
        mult | frac | div | modulo
      case 1 =>
        plus | minus
      case 2 =>
        max | equality | geq | leq | greater | less | or | and
    }
  }

  private lazy val unopexp: PackratParser[Exp] = positioned {
    (unop ~ expr(-1)) ^^ { case op ~ exp => UnOpExp(op, exp)}
  }

  private lazy val unop: PackratParser[UnOp] = positioned {
    neg | not
  }

  private lazy val ifelseexp: PackratParser[Exp] = positioned {
    (IFF() ~ LEFT_PAREN() ~ expr() ~ RIGHT_PAREN() ~ LEFT_BRACE() ~ expr() ~ RIGHT_BRACE() ~ EELSE() ~ LEFT_BRACE() ~ expr() ~ RIGHT_BRACE())
      ^^ { case _ ~ _ ~ cond ~ _ ~ _ ~ iftrue ~ _ ~ _ ~ _ ~ iffalse ~ _ => IfElseExp(cond, iftrue, iffalse)}
  }

  private lazy val loopexp: PackratParser[Decl] = positioned {
    (LOOP() ~ LEFT_PAREN() ~ expr() ~ RIGHT_PAREN() ~ unitblock)
      ^^ { case _ ~ _ ~ times ~ _ ~ exp => LoopExp(times, exp) }
  }

  private lazy val loopifexp: PackratParser[Decl] = positioned {
    (LOOP() ~ IFF() ~ LEFT_PAREN() ~ expr() ~ RIGHT_PAREN() ~ unitblock)
      ^^ { case _ ~ _ ~ _ ~ cond ~ _ ~ exp => LoopIfExp(cond, exp) }
  }

  private lazy val plus: PackratParser[BinOp] = OP("+") ^^ { _ => PlusBinOp() }

  private lazy val minus: PackratParser[BinOp] = OP("-") ^^ { _ => MinusBinOp() }

  private lazy val mult: PackratParser[BinOp] = OP("*") ^^ { _ => MultBinOp() }

  private lazy val frac: PackratParser[BinOp] = OP("//") ^^ { _ => FracBinOp() }

  private lazy val div: PackratParser[BinOp] = OP("/") ^^ { _ => DivBinOp() }

  private lazy val max: PackratParser[BinOp] = OP("max") ^^ { _ => MaxBinOp() }

  private lazy val modulo: PackratParser[BinOp] = OP("%") ^^ { _ => ModuloBinOp() }

  private lazy val equality: PackratParser[BinOp] = OP("==") ^^ { _ => EqualityBinOp() }

  private lazy val greater: PackratParser[BinOp] = OP(">") ^^ { _ => GreaterBinOp() }

  private lazy val geq: PackratParser[BinOp] = OP(">=") ^^ { _ => GreaterOrEqualBinOp() }

  private lazy val leq: PackratParser[BinOp] = OP("<=") ^^ { _ => LessOrEqualBinOp() }

  private lazy val less: PackratParser[BinOp] = OP("<") ^^ { _ => LessBinOp()}

  private lazy val neg: PackratParser[UnOp] = OP("-") ^^ { _ => NegUnOp() }

  private lazy val not: PackratParser[UnOp] = OP("!") ^^ { _ => NotUnOp() }

  private lazy val or: PackratParser[BinOp] = OP("|") ^^ { _ => OrBinOp() }

  private lazy val and: PackratParser[BinOp] = OP("&") ^^ { _ => AndBinOp() }


  private lazy val identifier: PackratParser[IDENTIFIER] =
    accept("identifier", { case id: IDENTIFIER => id })

  class CuTokenReader(tokens: Seq[CuToken]) extends Reader[CuToken] {
    override def first: CuToken = tokens.head
    override def atEnd: Boolean = tokens.isEmpty
    override def pos: Position = tokens.headOption.map(_.pos).getOrElse(NoPosition)
    override def rest: Reader[CuToken] = CuTokenReader(tokens.tail)
  }
}
