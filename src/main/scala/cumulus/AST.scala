package cumulus

import scala.util.parsing.input.{NoPosition, Position}

object AST {

  class CumulusError(msg: String, pos: Position = NoPosition)
    extends RuntimeException(if (pos != NoPosition) s"$msg at line ${pos.line} column ${pos.column}" else msg)
}
