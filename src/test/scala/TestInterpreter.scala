import cumulus.AST._
import org.scalatest.funsuite.AnyFunSuite
import cumulus.parser.Parser._
import cumulus.parser.Tokens._
import cumulus.Interpreter._

class TestInterpreter extends AnyFunSuite {
  test("1.2//2 evaluates to FracVal(FloatVal(1.2), IntVal(2))") {
    val code = "1.2//2"
    val out = eval(code)
    assert(out == FracVal(FloatVal(1.2), IntVal(2)))
  }
}
