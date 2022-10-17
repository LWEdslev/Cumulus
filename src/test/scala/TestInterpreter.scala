import cumulus.AST._
import org.scalatest.funsuite.AnyFunSuite
import cumulus.parser.Parser._
import cumulus.parser.Tokens._
import cumulus.Interpreter._

class TestInterpreter extends AnyFunSuite {
  test("1.2//2 evaluates to FracVal(FloatVal(1.2), IntVal(2))") {
    val code = "1.2//2"
    val out = evalNew(code)._1
    assert(out == FracVal(FloatVal(1.2), IntVal(2)))
  }

  test("\"_1//12345678*12345678_\" evaluates to 1") {
    val code = "_1//12345678*12345678_"
    val out = evalNew(code)._1
    assert(out == FloatVal(1))
  }

  test("\"_1//12345678_*12345678\" does not eval to 1") {
    val code = "_1//12345678_*12345678"
    val out = evalNew(code)._1
    assert(out != FloatVal(1))
  }
}
