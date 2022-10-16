import org.scalatest.funsuite.AnyFunSuite
import Parser.Lexer.*
import _root_.Parser.Tokens.*

class TestLexer extends AnyFunSuite {
  test("5 lexes to INT(5)") {
    val code = "5"
    val tokens = apply(code)
    assert(tokens.head == INT(5))
  }
}
