package cumulus

import scala.util.parsing.input.{NoPosition, Position, Positional}

object AST {

  sealed abstract class AstNode extends Positional

  type Var = String

  sealed abstract class DeclOrExp extends AstNode
    sealed abstract class Exp extends DeclOrExp
      case class VarExp(x: Var) extends Exp
      case class BinOpExp(left: Exp, op: BinOp, right: Exp) extends Exp
      case class UnOpExp(op: UnOp, exp: Exp) extends Exp
      case class BlockExp(vars: List[VarDecl], exp: Exp) extends Exp
      sealed abstract class Lit extends Exp
        case class IntLit(i: Int) extends Lit
        case class FloatLit(f: Float) extends Lit

    sealed abstract class BinOp extends AstNode
      case class PlusBinOp() extends BinOp
      case class MinusBinOp() extends BinOp
      case class MultBinOp() extends BinOp
      case class DivBinOp() extends BinOp
      case class FracBinOp() extends BinOp
      case class MaxBinOp() extends BinOp
      case class ModuloBinOp() extends BinOp

    sealed abstract class UnOp extends AstNode
      case class NegUnOp() extends UnOp
      case class SimplifyOp() extends UnOp

    case class VarDecl(x: Var, exp: Exp) extends DeclOrExp



  class CumulusError(msg: String, pos: Position = NoPosition)
    extends RuntimeException(if (pos != NoPosition) s"$msg at line ${pos.line} column ${pos.column}" else msg)
}
