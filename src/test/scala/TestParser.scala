import cumulus.AST._
import org.scalatest.funsuite.AnyFunSuite
import cumulus.parser.Parser._
import cumulus.parser.Tokens._

class TestParser extends AnyFunSuite {
  test("5 parses to IntLit(5)") {
    val code = "5"
    val ast = parse(code)
    assert(ast == IntLit(5))
  }

  test("(5) parses to IntLit(5)") {
    val code = "(5)"
    val ast = parse(code)
    assert(ast == IntLit(5))
  }

  test("1+2 parses to BinOpExp(IntLit(1), PlusBinOp(), IntLit(2))") {
    val code = "1+2"
    val ast = parse(code)
    assert(ast == BinOpExp(IntLit(1), PlusBinOp(), IntLit(2)))
  }

  test("1.2*2 parses to BinOpExp(FloatLit(1.2), MultBinOp(), IntLit(2))") {
    val code = "1.2*2"
    val ast = parse(code)
    assert(ast == BinOpExp(FloatLit(1.2), MultBinOp(), IntLit(2)))
  }

  test("1.2//2 parses to BinOpExp(FloatLit(1.2), FracBinOp(), IntLit(2))") {
    val code = "1.2//2"
    val ast = parse(code)
    assert(ast == BinOpExp(FloatLit(1.2), FracBinOp(), IntLit(2)))
  }
}
