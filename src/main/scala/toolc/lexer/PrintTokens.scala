package toolc
package lexer

import utils._
import scala.io.Source
import java.io.File

object PrintTokens extends Pipeline[Iterator[Token], Iterator[Token]] {
  import Tokens._

  def run(ctx: Context)(it: Iterator[Token]): Iterator[Token] = {
    val tokens = it.map { t =>
      println(s"$t(${t.line}:${t.col})");
      t
    }

    tokens.toList.iterator // consume the iterator and get an iterator over the result
  }
}
