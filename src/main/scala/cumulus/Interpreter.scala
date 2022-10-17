package cumulus

import cumulus.AST.*
import cumulus.parser.Parser

object Interpreter {

  sealed abstract class Val
  case class IntVal(v: Int) extends Val
  case class FloatVal(v: Float) extends Val
  case class FracVal(n: Val, d: Val) extends Val

  def eval(code: String): Val = eval(Parser.parse(code))

  def eval(e: Exp): Val = e match {
    case VarExp(x) => ???
    case BinOpExp(left, op, right) => {
      val leftVal = eval(left)
      val rightVal = eval(right)
      op match
        case PlusBinOp() =>
          (leftVal, rightVal) match
            case (IntVal(a), IntVal(b)) => IntVal(a + b)
            case (FloatVal(a), IntVal(b)) => FloatVal(a + b)
            case (IntVal(a), FloatVal(b)) => FloatVal(a + b)
            case (FloatVal(a), FloatVal(b)) => FloatVal(a + b)
            case (f1@FracVal(n1, d1), f2@FracVal(n2, d2)) =>
              FracVal(
                eval(BinOpExp(
                  BinOpExp(getNumerator(f1), MultBinOp(), getDenominator(f2)),
                  PlusBinOp(),
                  BinOpExp(getNumerator(f2), MultBinOp(), getDenominator(f1))
                )), eval(BinOpExp(getDenominator(f1), MultBinOp(), getDenominator(f2)))
              )
            case (a, f @ FracVal(n, d)) =>
              eval(BinOpExp(right, PlusBinOp(), left))
            case (f @ FracVal(_, d), IntVal(b)) =>
              FracVal(
                eval(BinOpExp(
                  getNumerator(f), PlusBinOp(), BinOpExp(IntLit(b), MultBinOp(), getDenominator(f)))), d)
            case (f @ FracVal(_, d), FloatVal(b)) =>
              FracVal(
                eval(BinOpExp(
                  getNumerator(f), PlusBinOp(), BinOpExp(FloatLit(b), MultBinOp(), getDenominator(f)))), d)
            case _ => ???
        case MinusBinOp() =>
          (leftVal, rightVal) match
            case (IntVal(a), IntVal(b)) => IntVal(a - b)
            case (FloatVal(a), IntVal(b)) => FloatVal(a - b)
            case (IntVal(a), FloatVal(b)) => FloatVal(a - b)
            case (FloatVal(a), FloatVal(b)) => FloatVal(a - b)
            case (f1@FracVal(n1, d1), f2@FracVal(n2, d2)) =>
              FracVal(
                eval(BinOpExp(
                  BinOpExp(getNumerator(f1), MultBinOp(), getDenominator(f2)),
                    MinusBinOp(),
                  BinOpExp(getNumerator(f2), MultBinOp(), getDenominator(f1))
                )), eval(BinOpExp(getDenominator(f1), MultBinOp(), getDenominator(f2)))
              )
            case (a, f@FracVal(n, d)) =>
              eval(BinOpExp(right, PlusBinOp(), left))
            case (f@FracVal(_, d), IntVal(b)) =>
              FracVal(
                eval(BinOpExp(
                  getNumerator(f), PlusBinOp(), BinOpExp(IntLit(-b), MultBinOp(), getDenominator(f)))), d)
            case (f@FracVal(_, d), FloatVal(b)) =>
              FracVal(
                eval(BinOpExp(
                  getNumerator(f), PlusBinOp(), BinOpExp(FloatLit(-b), MultBinOp(), getDenominator(f)))), d)
            case _ => ???
        case MultBinOp() =>
          (leftVal, rightVal) match
            case (IntVal(a), IntVal(b)) => IntVal(a * b)
            case (FloatVal(a), IntVal(b)) => FloatVal(a * b)
            case (IntVal(a), FloatVal(b)) => FloatVal(a * b)
            case (FloatVal(a), FloatVal(b)) => FloatVal(a * b)
            case (a, f@FracVal(n, d)) =>
              eval(BinOpExp(right, MultBinOp(), left))
            case (f1@FracVal(n1, d1), f2@FracVal(n2, d2)) =>
              FracVal(
                eval(BinOpExp(
                  getNumerator(f1), MultBinOp(), getNumerator(f2)
                )), eval(BinOpExp(getDenominator(f1), MultBinOp(), getDenominator(f2)))
              )
            case (f@FracVal(_, d), IntVal(b)) =>
              FracVal(
                eval(BinOpExp(
                  getNumerator(f), MultBinOp(), IntLit(b))), d)
            case (f@FracVal(_, d), FloatVal(b)) =>
              FracVal(
                eval(BinOpExp(
                  getNumerator(f), MultBinOp(), FloatLit(b))), d)
            case _ => ???
        case DivBinOp() =>
          if (rightVal == IntVal(0)) throw InterpreterError(s"you can't even math, trying to divide by zero", e)
          (leftVal, rightVal) match
            case (IntVal(a), IntVal(b)) => IntVal(a / b)
            case (FloatVal(a), IntVal(b)) => FloatVal(a / b)
            case (IntVal(a), FloatVal(b)) => FloatVal(a / b)
            case (FloatVal(a), FloatVal(b)) => FloatVal(a / b)
            case _ => ???
        case FracBinOp() => FracVal(leftVal, rightVal)
        case MaxBinOp() => ???
        case ModuloBinOp() => ???
    }
    case UnOpExp(op, exp) =>
      val value = eval(exp)
      op match
        case NegUnOp() =>
          value match
            case IntVal(i) => IntVal(-i)
            case FloatVal(f) => FloatVal(-f)
            case f @ FracVal(n, d) => FracVal(eval(getNumerator(f)), d)
            case _ => ???
        case SimplifyOp() => calculateFrac(value, e)
    case BlockExp(vars, exp) => ???
    case lit: Lit => lit match
      case IntLit(i) => IntVal(i)
      case FloatLit(f) => FloatVal(f)
  }

  def calculateFrac(v: Val, e: AstNode): Val = v match
    case f: FracVal => f match
      case FracVal(n: IntVal, d: IntVal) =>
        if (d.v == 0) throw InterpreterError(s"you can't even math, trying to divide by zero", e)
        FloatVal(n.v.toFloat / d.v.toFloat)
      case FracVal(n: IntVal, d: FloatVal) =>
        if (d.v == 0) throw InterpreterError(s"you can't even math, trying to divide by zero", e)
        FloatVal(n.v.toFloat / d.v)
      case FracVal(n: FloatVal, d: IntVal) =>
        if (d.v == 0) throw InterpreterError(s"you can't even math, trying to divide by zero", e)
        FloatVal(n.v / d.v.toFloat)
      case FracVal(n: FloatVal, d: FloatVal) =>
        if (d.v == 0) throw InterpreterError(s"you can't even math, trying to divide by zero", e)
        FloatVal(n.v / d.v)
      case FracVal(n: FracVal, d) =>
        d match
          case i: IntVal =>
            if (i.v == 0) throw InterpreterError(s"you can't even math, trying to divide by zero", e)
            val numerator = calculateFrac(n, e)
            calculateFrac(FracVal(numerator, d), e)
          case f: FloatVal =>
            if (f.v == 0.0f)
              throw InterpreterError(s"you can't even math, trying to divide by zero", e)
            val numerator = calculateFrac(n, e)
            calculateFrac(FracVal(numerator, d), e)
          case _ => throw Error("Unreachable")
      case FracVal(n, d: FracVal) =>
        n match
          case i: IntVal =>
            if (i.v == 0) throw InterpreterError(s"you can't even math, trying to divide by zero", e)
            val denominator = calculateFrac(d, e)
            calculateFrac(FracVal(n, denominator), e)
          case f: FloatVal =>
            if (f.v == 0.0f) throw InterpreterError(s"you can't even math, trying to divide by zero", e)
            val denominator = calculateFrac(d, e)
            calculateFrac(FracVal(n, denominator), e)
          case _ => throw Error("Unreachable")
    case v: Val => v

  private def getNumerator(f: FracVal): Exp = f match
    case FracVal(n, d) => n match
      case i: IntVal => IntLit(i.v)
      case f: FloatVal => FloatLit(f.v)
      case frac: FracVal => getNumerator(frac)
      case _ => ???

  private def getDenominator(f: FracVal): Exp = f match
    case FracVal(n, d) => d match
      case i: IntVal => IntLit(i.v)
      case f: FloatVal => FloatLit(f.v)
      case frac: FracVal => BinOpExp(getNumerator(f), FracBinOp(), getDenominator(f))
      case _ => ???
  class InterpreterError(msg: String, node: AstNode) extends CumulusError(s"you can't even program: $msg", node.pos)
}
