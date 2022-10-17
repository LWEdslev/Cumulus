package cumulus

import cumulus.AST.*
import cumulus.parser.Parser

object Interpreter {

  sealed abstract class Val

  case class IntVal(v: Int) extends Val

  case class FloatVal(v: Float) extends Val

  case class FracVal(n: Val, d: Val) extends Val

  case class RefVal(loc: Loc) extends Val

  type Env = Map[Var, Val]
  type Sto = Map[Loc, Val]
  type Loc = Int

  def nextLoc(sto: Sto): Loc = sto.size

  private def makeEnv(): Env = Map[Var, Val]()
  private def makeSto(): Sto = Map[Loc, Val]()

  def eval(code: String): (Val, Env, Sto) = eval(Parser.parse(code), makeEnv(), makeSto())

  def eval(e: Exp, env: Env, sto: Sto): (Val, Env, Sto) = e match {
    case VarExp(x) => env(x) match
      case RefVal(loc) => (sto(loc), env, sto)
      case _ => (env(x), env, sto)
    case VarDecl(x, exp) =>
      val expValue: (Val, Env, Sto) = eval(exp, env, sto)
      val sto1 = expValue._3
      val loc: Loc = nextLoc(sto1)
      val sto2 = sto1 + (loc -> expValue._1)
      val env1 = env + (x -> RefVal(loc))
      (expValue._1, env1, sto2)
    case BinOpExp(left, op, right) =>
      val (leftVal, env1, sto1) = eval(left, env, sto)
      val (rightVal, env2, sto2) = eval(right, env1, sto1)
      op match
        case PlusBinOp() =>
          (leftVal, rightVal) match
            case (IntVal(a), IntVal(b)) => (IntVal(a + b), env2, sto2)
            case (FloatVal(a), IntVal(b)) => (FloatVal(a + b), env2, sto2)
            case (IntVal(a), FloatVal(b)) => (FloatVal(a + b), env2, sto2)
            case (FloatVal(a), FloatVal(b)) => (FloatVal(a + b), env2, sto2)
            case (f1@FracVal(n1, d1), f2@FracVal(n2, d2)) =>
              val (newNumerator, env3, sto3) = eval(BinOpExp(
                BinOpExp(getNumerator(f1), MultBinOp(), getDenominator(f2)),
                PlusBinOp(),
                BinOpExp(getNumerator(f2), MultBinOp(), getDenominator(f1))
              ), env2, sto2)
              val (newDenominator, env4, sto4) = eval(BinOpExp(getDenominator(f1), MultBinOp(), getDenominator(f2)), env3, sto3)
              (FracVal(newNumerator, newDenominator), env4, sto4)
            case (a, f @ FracVal(n, d)) =>
              eval(BinOpExp(right, PlusBinOp(), left), env2, sto2)
            case (f @ FracVal(_, d), IntVal(b)) =>
              val (newNumerator, env3, sto3) = eval(BinOpExp(
                getNumerator(f), PlusBinOp(), BinOpExp(IntLit(b), MultBinOp(), getDenominator(f))), env2, sto2)
              (FracVal(newNumerator, d), env3, sto3)
            case (f @ FracVal(_, d), FloatVal(b)) =>
              val (newNumerator, env3, sto3) = eval(BinOpExp(
                getNumerator(f), PlusBinOp(), BinOpExp(FloatLit(b), MultBinOp(), getDenominator(f))), env2, sto2)
              (FracVal(newNumerator, d), env3, sto3)
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
