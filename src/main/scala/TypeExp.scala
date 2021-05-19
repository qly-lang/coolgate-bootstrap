sealed trait TypeExp {}

case class AddrType(member: TypeExp) extends TypeExp
case class FunType(params: Vector[TypeExp], returnType: TypeExp) extends TypeExp
case class ArrayType(member: TypeExp, len: BigInt) extends TypeExp
case class StructType(fields: Vector[Field]) extends TypeExp
case class UnionType(fields: Vector[Field]) extends TypeExp
case class Refer(to: TypeDef) extends TypeExp
// For resume from error in type analysis pass
case object ErrorType extends Throwable with TypeExp

case class Field(name: String, t: TypeExp)
