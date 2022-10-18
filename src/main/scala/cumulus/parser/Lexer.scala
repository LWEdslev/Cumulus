package cumulus.parser

import cumulus.AST.*
import cumulus.parser.Tokens.*

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.Position

object Lexer extends RegexParsers {
  override def skipWhitespace = true

  override val whiteSpace: Regex = """(?:\s|#.*|/\*(?:.|[\n\r])*?\*/)+""".r

  private val keywords = Set("true", "false", "max", "if", "else", "while", "fun", "let", "match", "case", "class", "new", "null", "do")

  def apply(code: String): List[CuToken] = {
    parse(tokens, code) match
      case Success(result, _) => result
      case p @ (NoSuccess(_, _) | Failure(_, _) | Error(_, _)) => throw SyntaxError(getPosition(p.next))
  }

  private def tokens: Parser[List[CuToken]] =
    phrase(
      rep1(
        op |
          literalBool |
          simpleType |
          literalNull |
          let |
          fun |
          iff |
          eelse |
          left_paren |
          right_paren |
          left_brace |
          right_brace |
          wwhile |
          arrow |
          eq |
          colon |
          comma |
          mmatch |
          ccase |
          cclass |
          dot |
          nnew |
          semicolon |
          identifier |
          literalString |
          literalFloat |
          literalInt
      )
    )


  private def identifier: Parser[IDENTIFIER] = positioned {
    "[a-zA-Z][a-zA-Z0-9]*".r ^^ { case str if !keywords.contains(str) => IDENTIFIER(str) }
  }

  private def literalString: Parser[STRING] = positioned {
    """"[^"]*"""".r ^^ { str => STRING(str.substring(1, str.length - 1)) }
  }

  private def literalInt: Parser[INT] = positioned {
    """[0-9]+""".r ^^ { lit => INT(lit.toInt) }
  }

  private def literalFloat: Parser[FLOAT] = positioned {
    """[0-9]+(\.[0-9]+)f?""".r ^^ { lit => FLOAT(lit.toFloat) }
  }

  private def literalBool: Parser[BOOL] = positioned {
    """(true|false)\b""".r ^^ { lit => BOOL(lit.toBoolean) }
  }

  private def literalNull: Parser[NULL] = positioned {
    """null\b""".r ^^ { lit => NULL() }
  }

  private def op: Parser[OP] = positioned {
    """\+|\*|_|-|//|/|<=|==|<|%|!|\|\||\||&&|&|max""".r ^^ { op => OP(op) }
  }

  private def simpleType: Parser[SIMPLE_TYPE] = positioned {
    """(String|Int|Float|Boolean|Unit|Null)""".r ^^ { lit => SIMPLE_TYPE(lit) }
  }

  private def let: Parser[LET] = positioned {
    """let\b""".r ^^ { _ => LET() }
  }

  private def fun: Parser[FUN] = positioned {
    """fun\b""".r ^^ { _ => FUN() }
  }

  private def wwhile: Parser[WWHILE] = positioned {
    """while\b""".r ^^ { _ => WWHILE() }
  }

  private def iff: Parser[IFF] = positioned {
    """if\b""".r ^^ { _ => IFF() }
  }

  private def eelse: Parser[EELSE] = positioned {
    """else\b""".r ^^ { _ => EELSE() }
  }

  private def left_paren: Parser[LEFT_PAREN] = positioned {
    "(" ^^ { _ => LEFT_PAREN() }
  }

  private def right_paren: Parser[RIGHT_PAREN] = positioned {
    ")" ^^ { _ => RIGHT_PAREN() }
  }

  private def left_brace: Parser[LEFT_BRACE] = positioned {
    "{" ^^ { _ => LEFT_BRACE() }
  }

  private def right_brace: Parser[RIGHT_BRACE] = positioned {
    "}" ^^ { _ => RIGHT_BRACE() }
  }

  private def eq: Parser[EQ] = positioned {
    "=" ^^ { _ => EQ() }
  }

  private def colon: Parser[COLON] = positioned {
    ":" ^^ { _ => COLON() }
  }

  private def comma: Parser[COMMA] = positioned {
    "," ^^ { _ => COMMA() }
  }

  private def semicolon: Parser[SEMICOLON] = positioned {
    ";" ^^ { _ => SEMICOLON() }
  }

  private def arrow: Parser[ARROW] = positioned {
    "=>" ^^ { _ => ARROW() }
  }

  private def mmatch: Parser[MATCH] = positioned {
    """match\b""".r ^^ { _ => MATCH() }
  }

  private def ccase: Parser[CASE] = positioned {
    """case\b""".r ^^ { _ => CASE() }
  }

  private def cclass: Parser[CLASS] = positioned {
    """class\b""".r ^^ { _ => CLASS() }
  }

  private def dot: Parser[DOT] = positioned {
    "." ^^ { _ => DOT() }
  }

  private def nnew: Parser[NEW] = positioned {
    """new\b""".r ^^ { _ => NEW() }
  }

  private def getPosition(i: Lexer.Input) = SyntheticPosition(i.pos.line, i.pos.column)

  private case class SyntheticPosition(line: Int, column: Int, lineContents: String = "") extends Position

  class SyntaxError(pos: Position) extends CumulusError("what you wrote here makes no fucking sense: ", pos)
}
