import java.nio.file.Path
import scala.util.parsing.combinator.{PackratParsers, RegexParsers}
import scala.util.parsing.input.{CharSequenceReader, Reader}

case class Location(line: Int, column: Int) {
  override def toString = s"line: $line, column: $column"
}

object Parser extends RegexParsers with PackratParsers {
  override def skipWhitespace: Boolean = false

  // TODO: human understandable error message
  @throws(classOf[SyntaxError])
  def apply(code: String): AST = {
    val reader = new PackratReader(new CharSequenceReader(code))
    phrase(mexps)(reader) match {
      case NoSuccess(msg, next) => {
        throw SyntaxError(Location(next.pos.line, next.pos.column), msg)
      }
      case Success(result, next) => new AST(result.toVector)
    }
  }

  @throws(classOf[SyntaxError])
  def apply(path: Path): AST = {
    val file = scala.io.Source.fromFile(path.toString)
    val result = apply(file.mkString)
    file.close()
    result
  }

  def mexps: Parser[List[MExp]] = {
    whitespaceAndMexp.* <~ whitespace.?
  }

  def whitespaceAndMexp: Parser[MExp] = {
    whitespace.? ~> mexp
  }

  lazy val mexp: PackratParser[MExp] = positioned {
    colonExpAndHigher
  }

  lazy val colonExpAndHigher: PackratParser[MExp] = positioned {
    colonExp | opDotExpAndHigher
  }

  lazy val colonExp: PackratParser[ColonExp] = positioned {
    opDotExpAndHigher ~ (whitespace.? ~> ':' <~ whitespace.?) ~ colonExpAndHigher ^^ {
      case v ~ _ ~ c => ColonExp(v, c)
    }
  }

  lazy val opDotExpAndHigher: PackratParser[MExp] = positioned {
    opExp | dotExp | primaryExp
  }

  lazy val opExp: PackratParser[OpExp] = positioned {
    opDotExpAndHigher ~ arrayExp ^^ {
      case v ~ a => OpExp(v, a.elems)
    }
  }

  lazy val dotExp: PackratParser[DotExp] = positioned {
    opDotExpAndHigher ~ (whitespace.? ~> '.' <~ whitespace.?) ~ primaryExp ^^ {
      case v ~ _ ~ d => DotExp(v, d)
    }
  }

  def primaryExp: Parser[MExp] =
    positioned {
      arrayExp | atom
    }

  def arrayExp: Parser[ArrayExp] =
    positioned {
      '[' ~ mexps ~ ']' ^^ {
        case _ ~ elems ~ _ => ArrayExp(elems.toVector)
      }
    }

  def atom: Parser[Atom] =
    positioned {
      string | float | uint | int | symbol
    }

  def string: Parser[StringLit] =
    positioned {
      """"((\\[\d\D])|([^"]))*"""".r ^^ { s =>
        StringLit(s.drop(1).dropRight(1).replaceAll("""\\([\d\D])""", "$1"))
      }
    }

  def float: Parser[FloatLit] =
    positioned {
      """[-+]?([0-9]*[.][0-9]*([eE][-+]?[0-9]+)?)|([0-9]+[eE][-+]?[0-9]+)""".r <~ guard(
        not(symbolChar)
      ) ^^ { s => FloatLit(s.toFloat) }
    }

  def int: Parser[IntLit] =
    positioned {
      """[-+]?[0-9]+""".r <~ guard(not(symbolChar)) ^^ { s => IntLit(s.toLong) }
    }

  def uint: Parser[UIntLit] =
    positioned {
      """(0x[0-9a-f]+)|(0o[0-7]+)|(0b[01]+)""".r <~ guard(not(symbolChar)) ^^ {
        s =>
          {
            val b = s.drop(1).head
            val base = if (b == 'x') 16 else if (b == 'o') 8 else 2
            val u = BigInt(s.drop(2), base)
            UIntLit(u)
          }
      }
    }

  def symbol: Parser[SymbolLit] =
    positioned {
      symbolChar.+ ^^ { s =>
        SymbolLit(s.mkString.replaceAll("""\\([\d\D])""", "$1"))
      }
    }

  def symbolChar: Parser[Char] = {
    """\\[\d\D]""".r ^^ { escapeChar =>
      escapeChar.charAt(1)
    } | """[^\s.\[\]:"\\',@#]""".r ^^ { char =>
      char.head
    }
  }

  def whitespace: Parser[Unit] = {
    ("""[\s]""".r | comment).+ ^^^ ()
  }

  def comment: Parser[Unit] = {
    """#.*""".r ~ '\n'.? ^^^ ()
  }
}
