package cumulus

import cumulus.AST.*
import cumulus.parser.Parser

import scala.annotation.tailrec

object Interpreter {

  sealed abstract class Val

  case class IntVal(v: Int) extends Val

  case class FloatVal(v: Float) extends Val

  case class BoolVal(b: Boolean) extends Val

  case class FracVal(n: Val, d: Val) extends Val

  case class RefVal(loc: Loc) extends Val

  case class FunVal(params: List[Var], exp: Exp, env: Env, sto: Sto) extends Val

  case class UnitVal() extends Val

  type Env = Map[Var, Val]
  type Sto = Map[Loc, Val]
  type Loc = Int

  def nextLoc(sto: Sto): Loc = sto.size

  private def makeEnv(): Env = Map[Var, Val]()
  private def makeSto(): Sto = Map[Loc, Val]()

  def evalNew(code: String): (Val, Env, Sto) = eval(Parser.parse(code), makeEnv(), makeSto())

  def eval(e: Exp, env: Env, sto: Sto): (Val, Env, Sto) = e match {
    case UnitExp() => (UnitVal(), env, sto)
    case IfElseExp(cond, ifTrue, ifFalse) =>
      val (evalCond, env1, sto1) = eval(cond, env, sto)
      evalCond match
        case BoolVal(true) => eval(ifTrue, env1, sto1)
        case BoolVal(false) => eval(ifFalse, env1, sto1)
        case _ => throw InterpreterError(s"$cond does not evaluate to a boolean, idiot", e)
    case AssignExp(id, exp) =>
      if (env.contains(id)) {
        env(id) match
          case RefVal(loc) =>
            val (newVal, env1, sto1) = eval(exp, env, sto)
            (UnitVal(), env1, sto1 + (loc -> newVal))
          case _ => throw InterpreterError(s"are you mad? $id is not mutable", e)
      } else {
        throw InterpreterError(s"what made you think that you could re-assign $id?", e)
      }
    case LoopExp(times, exp) =>
      val (valTimes, env1, sto1) = eval(times, env, sto)
      valTimes match
        case IntVal(v) =>
          var env2 = env1
          var sto2 = sto1
          for (i <- 0 until v) {
            val (_, env3, sto3) = eval(exp, env2, sto2)
            env2 = env3
            sto2 = sto3
          }
          (UnitVal(), env2, sto2)
        case FloatVal(v) =>
          var env2 = env1
          var sto2 = sto1
          for (i <- 0 until v.toInt) {
            val (_, env3, sto3) = eval(exp, env2, sto2)
            env2 = env3
            sto2 = sto3
          }
          (UnitVal(), env2, sto2)
        case _ => throw InterpreterError(s"$times must be a float or int", e)
    case LoopIfExp(cond, exp) =>
      val (valCond, env1, sto1) = eval(cond, env, sto)
      valCond match
        case BoolVal(true) =>
          val (_, env2, sto2) = eval(exp, env1, sto1)
          eval(LoopIfExp(cond, exp), env2, sto2)
        case BoolVal(false) => (UnitVal() , env1, sto1)
        case _ => throw InterpreterError(s"$cond does not evaluate to a boolean", e)
    case VarExp(x) => env(x) match
      case RefVal(loc) => (sto(loc), env, sto)
      case _ => (env(x), env, sto)
    case VarDecl(x, exp) =>
      val expValue: (Val, Env, Sto) = eval(exp, env, sto)
      val sto1 = expValue._3
      val loc: Loc = nextLoc(sto1)
      val sto2 = sto1 + (loc -> expValue._1)
      val env1 = env + (x -> RefVal(loc))
      (UnitVal(), env1, sto2)
    case FunDecl(id: Var, params: List[Var], exp: Exp) =>
      (UnitVal(), env + (id -> FunVal(params, exp, env, sto)), sto)
    case FunExp(id: Var, args: List[Exp]) => env(id) match
      case FunVal(params, exp, env, sto) => {
        if (params.length != args.length)
          throw InterpreterError(s"there are ${params.length} parameters and ${args.length}, you dumb fuck", e)
        var env1 = env
        var sto1 = sto
        for (a <- args.zip(params)) {
          val (argVal, env2, sto2) = eval(a._1, env1, sto1)
          env1 = env2 + (a._2 -> argVal)
          sto1 = sto2
        }
        eval(exp, env1, sto1)
      }
      case _ => throw InterpreterError(s"$id is not defined as a function, but who would expect you to understand", e)
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
            case (f1@FracVal(_, _), f2@FracVal(_, _)) =>
              val (newNumerator, env3, sto3) = eval(BinOpExp(
                BinOpExp(getNumerator(f1), MultBinOp(), getDenominator(f2)),
                PlusBinOp(),
                BinOpExp(getNumerator(f2), MultBinOp(), getDenominator(f1))
              ), env2, sto2)
              val (newDenominator, env4, sto4) = eval(BinOpExp(getDenominator(f1), MultBinOp(), getDenominator(f2)), env3, sto3)
              (FracVal(newNumerator, newDenominator), env4, sto4)
            case (_, FracVal(_, _)) =>
              eval(BinOpExp(right, PlusBinOp(), left), env2, sto2)
            case (f @ FracVal(_, d), IntVal(b)) =>
              val (newNumerator, env3, sto3) = eval(BinOpExp(
                getNumerator(f), PlusBinOp(), BinOpExp(IntLit(b), MultBinOp(), getDenominator(f))), env2, sto2)
              (FracVal(newNumerator, d), env3, sto3)
            case (f @ FracVal(_, d), FloatVal(b)) =>
              val (newNumerator, env3, sto3) = 
                eval(BinOpExp(getNumerator(f), PlusBinOp(), 
                  BinOpExp(FloatLit(b), MultBinOp(), getDenominator(f))), env2, sto2)
              (FracVal(newNumerator, d), env3, sto3)
            case _ => throw InterpreterError(s"adding $left and $right? you can't idiot", e)
        case MinusBinOp() =>
          (leftVal, rightVal) match
            case (IntVal(a), IntVal(b)) => (IntVal(a - b), env2, sto2)
            case (FloatVal(a), IntVal(b)) => (FloatVal(a - b), env2, sto2)
            case (IntVal(a), FloatVal(b)) => (FloatVal(a - b), env2, sto2)
            case (FloatVal(a), FloatVal(b)) => (FloatVal(a - b), env2, sto2)
            case (f1@FracVal(_, _), f2@FracVal(_, _)) =>
              val (newNumerator, env3, sto3) = eval(BinOpExp(
                BinOpExp(getNumerator(f1), MultBinOp(), getDenominator(f2)),
                MinusBinOp(),
                BinOpExp(getNumerator(f2), MultBinOp(), getDenominator(f1))
              ), env2, sto2)
              val (newDenominator, env4, sto4) = eval(BinOpExp(getDenominator(f1), MultBinOp(), getDenominator(f2)), env3, sto3)
              (FracVal(newNumerator, newDenominator), env4, sto4)
            case (_, FracVal(_, _)) => 
              eval(BinOpExp(right, PlusBinOp(), left), env2, sto2)
            case (f@FracVal(_, d), IntVal(b)) =>
              val (newNumerator, env3, sto3) = eval(BinOpExp(
                getNumerator(f), PlusBinOp(), BinOpExp(IntLit(-b), MultBinOp(), getDenominator(f))), env2, sto2)
              (FracVal(newNumerator, d), env3, sto3)
            case (f@FracVal(_, d), FloatVal(b)) =>
              val (newNumerator, env3, sto3) = eval(BinOpExp(
                getNumerator(f), PlusBinOp(), BinOpExp(FloatLit(-b), MultBinOp(), getDenominator(f))), env2, sto2)
              (FracVal(newNumerator, d), env3, sto3)
            case _ => throw InterpreterError(s"subtracting $left and $right? you can't idiot", e)
        case MultBinOp() =>
          (leftVal, rightVal) match
            case (IntVal(a), IntVal(b)) => (IntVal(a * b), env2, sto2)
            case (FloatVal(a), IntVal(b)) => (FloatVal(a * b), env2, sto2)
            case (IntVal(a), FloatVal(b)) => (FloatVal(a * b), env2, sto2)
            case (FloatVal(a), FloatVal(b)) => (FloatVal(a * b), env2, sto2)
            case (_, FracVal(_, _)) =>
              eval(BinOpExp(right, MultBinOp(), left), env2, sto2)
            case (f1@FracVal(_, _), f2@FracVal(_, _)) =>
              val (newNumerator, env3, sto3) = eval(BinOpExp(
                getNumerator(f1), MultBinOp(), getNumerator(f2)
              ), env2, sto2)
              val (newDenominator, env4, sto4) = eval(BinOpExp(getDenominator(f1), MultBinOp(), getDenominator(f2)), env3, sto3)
              (FracVal(newNumerator, newDenominator), env4, sto4)
            case (f@FracVal(_, d), IntVal(b)) =>
              val (newNumerator, env3, sto3) = eval(BinOpExp(
                getNumerator(f), MultBinOp(), IntLit(b)), env2, sto2)
              (FracVal(newNumerator, d), env3, sto3)
            case (f@FracVal(_, d), FloatVal(b)) =>
              val (newNumerator, env3, sto3) = eval(BinOpExp(
                getNumerator(f), MultBinOp(), FloatLit(b)), env2, sto2)
              (FracVal(newNumerator, d), env3, sto3)
            case _ => throw InterpreterError(s"$left times $right, just no...", e)
        case DivBinOp() =>
          if (rightVal == IntVal(0)) throw InterpreterError(s"you can't even math, trying to divide by zero", e)
          (leftVal, rightVal) match
            case (IntVal(a), IntVal(b)) => (IntVal(a / b), env2, sto2)
            case (FloatVal(a), IntVal(b)) => (FloatVal(a / b), env2, sto2)
            case (IntVal(a), FloatVal(b)) => (FloatVal(a / b), env2, sto2)
            case (FloatVal(a), FloatVal(b)) => (FloatVal(a / b), env2, sto2)
            case _ => throw InterpreterError(s"$left divided by $right?? you dumb fuck", e)
        case FracBinOp() => (FracVal(leftVal, rightVal), env2, sto2)
        case MaxBinOp() =>
          (leftVal, rightVal) match
            case (IntVal(a), IntVal(b)) => (IntVal(if (a > b) a else b), env2, sto2)
            case (FloatVal(a), IntVal(b)) => (FloatVal(if (a > b) a else b), env2, sto2)
            case (IntVal(a), FloatVal(b)) => (FloatVal(if (a > b) a else b), env2, sto2)
            case (FloatVal(a), FloatVal(b)) => (FloatVal(if (a > b) a else b), env2, sto2)
        case ModuloBinOp() =>
          if (rightVal == IntVal(0)) throw InterpreterError(s"you can't even math, trying to modulo by zero", e)
          (leftVal, rightVal) match
            case (IntVal(a), IntVal(b)) => (IntVal(a % b), env2, sto2)
            case (FloatVal(a), IntVal(b)) => (FloatVal(a % b), env2, sto2)
            case (IntVal(a), FloatVal(b)) => (FloatVal(a % b), env2, sto2)
            case (FloatVal(a), FloatVal(b)) => (FloatVal(a % b), env2, sto2)
            case _ => throw InterpreterError(s"$left modulo by $right?? you dumb fuck", e)
        case EqualityBinOp() =>
          (leftVal, rightVal) match
            case (IntVal(a), IntVal(b)) => (BoolVal(a == b), env2, sto2)
            case (FloatVal(a), FloatVal(b)) => (BoolVal(a == b), env2, sto2)
            case (FloatVal(a), IntVal(b)) => (BoolVal(a == b), env2, sto2)
            case (IntVal(a), FloatVal(b)) => (BoolVal(a == b), env2, sto2)
            case _ => throw InterpreterError(s"you can't compare $leftVal with $rightVal, why did you think that you could", e)
        case GreaterBinOp() =>
          (leftVal, rightVal) match
            case (IntVal(a), IntVal(b)) => (BoolVal(a > b), env2, sto2)
            case (FloatVal(a), FloatVal(b)) => (BoolVal(a > b), env2, sto2)
            case (FloatVal(a), IntVal(b)) => (BoolVal(a > b), env2, sto2)
            case (IntVal(a), FloatVal(b)) => (BoolVal(a > b), env2, sto2)
            case _ => throw InterpreterError(s"you can't compare $leftVal with $rightVal, why did you think that you could", e)
        case LessBinOp() =>
          (leftVal, rightVal) match
            case (IntVal(a), IntVal(b)) => (BoolVal(a < b), env2, sto2)
            case (FloatVal(a), FloatVal(b)) => (BoolVal(a < b), env2, sto2)
            case (FloatVal(a), IntVal(b)) => (BoolVal(a < b), env2, sto2)
            case (IntVal(a), FloatVal(b)) => (BoolVal(a < b), env2, sto2)
            case _ => throw InterpreterError(s"you can't compare $leftVal with $rightVal, why did you think that you could", e)
        case GreaterOrEqualBinOp() =>
          (leftVal, rightVal) match
            case (IntVal(a), IntVal(b)) => (BoolVal(a >= b), env2, sto2)
            case (FloatVal(a), FloatVal(b)) => (BoolVal(a >= b), env2, sto2)
            case (FloatVal(a), IntVal(b)) => (BoolVal(a >= b), env2, sto2)
            case (IntVal(a), FloatVal(b)) => (BoolVal(a >= b), env2, sto2)
            case _ => throw InterpreterError(s"you can't compare $leftVal with $rightVal, why did you think that you could", e)
        case LessOrEqualBinOp() =>
          (leftVal, rightVal) match
            case (IntVal(a), IntVal(b)) => (BoolVal(a <= b), env2, sto2)
            case (FloatVal(a), FloatVal(b)) => (BoolVal(a <= b), env2, sto2)
            case (FloatVal(a), IntVal(b)) => (BoolVal(a <= b), env2, sto2)
            case (IntVal(a), FloatVal(b)) => (BoolVal(a <= b), env2, sto2)
            case _ => throw InterpreterError(s"you can't compare $leftVal with $rightVal, why did you think that you could", e)
        case OrBinOp() =>
          (leftVal, rightVal) match
            case (BoolVal(a), BoolVal(b)) => (BoolVal(a || b), env2, sto2)
            case _ => throw InterpreterError(s"you can't compare $leftVal with $rightVal, why did you think that you could", e)
        case AndBinOp() =>
          (leftVal, rightVal) match
            case (BoolVal(a), BoolVal(b)) => (BoolVal(a && b), env2, sto2)
            case _ => throw InterpreterError(s"you can't compare $leftVal with $rightVal, why did you think that you could", e)
    case UnOpExp(op, exp) =>
      val (value, env1, sto1) = eval(exp, env, sto)
      op match
        case NegUnOp() =>
          value match
            case IntVal(i) => (IntVal(-i), env1, sto1)
            case FloatVal(f) => (FloatVal(-f), env1, sto1)
            case f @ FracVal(n, d) => 
              val (newNumerator, env2, sto2) = eval(getNumerator(f), env1, sto1)
              (FracVal(newNumerator, d), env2, sto2)
            case _ => throw InterpreterError(s"trying to negate $value, which makes no sense", e)
        case SimplifyOp() => (calculateFrac(value, e), env1, sto1)
        case NotUnOp() =>
          value match
            case BoolVal(b) => (BoolVal(!b), env1, sto1)
            case _ => throw InterpreterError(s"trying to negate $value, which makes no sense", e)
    case BlockExp(vars, exp) =>
      var env1 = env
      var sto1 = sto
      for (v <- vars) {
        val value = eval(v, env1, sto1)
        env1 = value._2
        sto1 = value._3
      }
      eval(exp, env1, sto1)
    case lit: Lit => lit match
      case IntLit(i) => (IntVal(i), env, sto)
      case FloatLit(f) => (FloatVal(f), env, sto)
      case BoolLit(b) => (BoolVal(b), env, sto)
  }

  @tailrec
  def evalPretty(code: String): String = evalNew(code)._1 match
    case IntVal(v) => v.toString
    case FloatVal(v) => v.toString
    case BoolVal(b) => b.toString
    case FracVal(_, _) => evalPretty(s"_$code _")
    case _ => throw new Error(s"the output can not be printed")

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
      case _ => throw Error("this should be unreachable")
    case v: Val => v

  @tailrec
  private def getNumerator(f: FracVal): Exp = f match
    case FracVal(n, d) => n match
      case i: IntVal => IntLit(i.v)
      case f: FloatVal => FloatLit(f.v)
      case frac: FracVal => getNumerator(frac)
      case _ => throw Error("this should be unreachable")

  private def getDenominator(f: FracVal): Exp = f match
    case FracVal(n, d) => d match
      case i: IntVal => IntLit(i.v)
      case f: FloatVal => FloatLit(f.v)
      case frac: FracVal => BinOpExp(getNumerator(f), FracBinOp(), getDenominator(f))
      case _ => throw Error("this should be unreachable")

  class InterpreterError(msg: String, node: AstNode) extends CumulusError(s"you can't even program: $msg", node.pos)
}
