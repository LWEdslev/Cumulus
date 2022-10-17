package cumulus.parser

import scala.util.parsing.input.Positional

object Tokens {
  sealed trait CuToken extends Positional

  case class OP(str: String) extends CuToken

  case class IDENTIFIER(str: String) extends CuToken

  case class SIMPLE_TYPE(str: String) extends CuToken

  case class STRING(str: String) extends CuToken

  case class INT(i: Int) extends CuToken

  case class BOOL(b: Boolean) extends CuToken

  case class FLOAT(v: Float) extends CuToken

  case class NULL() extends CuToken

  case class LET() extends CuToken

  case class FUN() extends CuToken

  case class DO() extends CuToken

  case class WWHILE() extends CuToken

  case class IFF() extends CuToken

  case class EELSE() extends CuToken

  case class LEFT_PAREN() extends CuToken

  case class RIGHT_PAREN() extends CuToken

  case class LEFT_BRACE() extends CuToken

  case class RIGHT_BRACE() extends CuToken

  case class EQ() extends CuToken

  case class COLON() extends CuToken

  case class COMMA() extends CuToken

  case class SEMICOLON() extends CuToken

  case class ARROW() extends CuToken

  case class MATCH() extends CuToken

  case class CASE() extends CuToken

  case class CLASS() extends CuToken

  case class DOT() extends CuToken

  case class NEW() extends CuToken
  
  case class UNDERSCORE() extends CuToken
}
