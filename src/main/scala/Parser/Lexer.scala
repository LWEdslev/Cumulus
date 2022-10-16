package Parser

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import Tokens._

object Lexer extends RegexParsers {
  override def skipWhitespace = true;

  override val whiteSpace: Regex = """[ \t\r\f\n]+""".r

  private val keywords = Set("true", "false", "max", "if", "else", "while", "fun", "let", "match", "case", "class", "new", "null", "do")

  def apply(code: String): List[CuToken] = {
    parse(tokens, code) match
      case Success(result, _) => result
      case _: NoSuccess => ??? //Implement syntax error here
  }

  private def tokens: Parser[List[CuToken]] =
    phrase(
      rep1(
        identifier |
          literalString |
          literalInt
      )
    )


  private def identifier: Parser[IDENTIFIER] = positioned {
    "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { case str if !keywords.contains(str) => IDENTIFIER(str) }
  }

  private def literalString: Parser[STRING] = positioned {
    """"[^"]*"""".r ^^ { str => STRING(str.substring(1, str.length - 1)) }
  }

  private def literalInt: Parser[INT] = positioned {
    """[0-9]+""".r ^^ { lit => INT(lit.toInt)}
  }


}
