import org.scalatest.funsuite.AnyFunSuite
import cumulus.parser.Lexer.*
import cumulus.parser.Tokens.*

class TestLexer extends AnyFunSuite {
  test("5 lexes to INT(5)") {
    val code = "5"
    val tokens = apply(code)
    assert(tokens.head == INT(5))
  }

  test(""""String" lexes to STRING("String")""") {
    val code = """"String""""
    val tokens = apply(code)
    assert(tokens.head == STRING("String"))
  }

  test("""x lexes to LITERAL("x")""") {
    val code = "x"
    val tokens = apply(code)
    assert(tokens.head == IDENTIFIER("x"))
  }

  test("1.2 lexes to FLOAT(1.2)") {
    val code = "1.2"
    val tokens = apply(code)
    assert(tokens.head == FLOAT(1.2))
  }

  test("1.2f lexes to FLOAT(1.2)") {
    val code = "1.2f"
    val tokens = apply(code)
    assert(tokens.head == FLOAT(1.2))
  }

  test("+ lexes to OP(\"+\") ") {
    val code = "+"
    val tokens = apply(code)
    assert(tokens.head == OP("+"))
  }

  test("""1+2 lexes to INT(1), OP("+"), INT(2)""") {
    val code = "1+2"
    val tokens = apply(code)
    assert(tokens == List(INT(1), OP("+"), INT(2)))
  }

  test("""true lexes to BOOL(true)""") {
    val code = "true"
    val tokens = apply(code)
    assert(tokens.head == BOOL(true))
  }

  test("""space true lexes to BOOL(true)""") {
    val code = " true"
    val tokens = apply(code)
    assert(tokens.head == BOOL(true))
  }

  test("""true+1 lexes to BOOL(true),OP("+"),INT(1)""") {
    val code = "true+1"
    val tokens = apply(code)
    assert(tokens == List(BOOL(true),OP("+"),INT(1)))
  }

  test("""true"2+1"1 lexes to BOOL(true),STRING("2+1"),INT(1)""") {
    val code = """true"2+1"1"""
    val tokens = apply(code)
    assert(tokens == List(BOOL(true), STRING("2+1"), INT(1)))
  }

  test("true1 lexes to identifier") {
    val code = "true1"
    val tokens = apply(code)
    assert(tokens.head == IDENTIFIER("true1"))
  }

  test("\"max\" gives STRING(\"max\")") {
    val code = """"max""""
    val tokens = apply(code)
    assert(tokens.head == STRING("max"))
  }

  test("String gives SIMPLE_TYPE(String)") {
    val code = "String"
    val tokens = apply(code)
    assert(tokens.head == SIMPLE_TYPE("String"))
  }

  test("null gives NULL") {
    val code = "null"
    val tokens = apply(code)
    assert(tokens.head == NULL())
  }

  test("let fun null gives LET(),FUN(),NULL()") {
    val code = "let fun null"
    val tokens = apply(code)
    assert(tokens == List(LET(),FUN(),NULL()))
  }
  
  test("=>match case class .new gives correct tokens") {
    val code = "=>match case class .new"
    val tokens = apply(code)
    assert(tokens == List(ARROW(), MATCH(), CASE(), CLASS(), DOT(), NEW()))
  }

  test(""""# comment" lexes to nothing""") {
    val code = "# comment\n1"
    val tokens = apply(code)
    assert(tokens.head == INT(1))
  }

  test("""// lexes to OP("//")""") {
    val code = "//"
    val tokens = apply(code)
    assert(tokens.head == OP("//"))
  }

  test("""/* multiline \n \n comment */ lexes to INT(1)""") {
    val code = "/* multiline \n \n comment */1"
    val tokens = apply(code)
    assert(tokens.head == INT(1))
  }
}
