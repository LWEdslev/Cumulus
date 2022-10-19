package cumulus

import java.io.File
import scala.io.Source

object Files {
  def readFile(directory: String): String = {
    val source = scala.io.Source.fromFile(directory)
    val lines = try source.mkString finally source.close()
    lines
  }
}
