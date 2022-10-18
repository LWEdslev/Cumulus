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

  test("\\_1.2//4//0.25\\_ evaluates to 1.2") {
    val code = "_1.2//4//0.25_"
    val out = evalNew(code)._1
    assert(out == FloatVal(1.2))
  }

  test("{let x = 5;let y = 10; x * y} evaluates to 50") {
    val code = "{let x = 5;let y = 10; x * y}"
    val out = evalNew(code)._1
    assert(out == IntVal(50))
  }

  test("{let x = 5;let y = 10; x * y}*2 evaluates to 100") {
    val code = "{let x = 5;let y = 10; x * y}*2"
    val out = evalNew(code)._1
    assert(out == IntVal(100))
  }

  test("{fun f(x) = 10*x; f(2)} evaluates to 20") {
    val code = "{fun f(x) = 10*x; f(2)}"
    val out = evalNew(code)._1
    assert(out == IntVal(20))
  }
}
