import scala.util.parsing.input.Positional
import io.bullet.borer.derivation.MapBasedCodecs._
import io.bullet.borer.{Codec, AdtEncodingStrategy}
import io.bullet.borer.Cbor

case class AST(val exps: Vector[MExp])

sealed trait MExp extends Positional

case class DotExp(exp: MExp, dot: MExp) extends MExp
case class ColonExp(exp: MExp, col: MExp) extends MExp
case class OpExp(op: MExp, args: Vector[MExp]) extends MExp
case class ArrayExp(elems: Vector[MExp]) extends MExp
sealed trait Atom extends MExp

case class StringLit(value: String) extends Atom
case class FloatLit(value: Float) extends Atom
case class IntLit(value: Long) extends Atom
case class UIntLit(value: BigInt) extends Atom
case class SymbolLit(value: String) extends Atom

object EncodeAST {
  implicit val flatAdtEncoding = {
    AdtEncodingStrategy.flat(typeMemberName = "kind")
  }
  implicit lazy val mexpCodec: Codec[MExp] = deriveAllCodecs[MExp]
  implicit val astCodec = deriveEncoder[AST]

  def apply(ast: AST): Array[Byte] = {
    Cbor.encode(ast).toByteArray
  }
}
