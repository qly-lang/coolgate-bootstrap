class CompilationError extends Throwable

case class SyntaxError(location: Location, msg: String) extends Throwable {
  override def toString: String = s"Syntax Error: $msg at $location"
}

trait SemanticError extends CompilationError

case object UndefinedType extends SemanticError
case class MalformedType(msg: String) extends SemanticError
case class MalformedOp(msg: String) extends SemanticError
case class TypeAlreadyDefinedInScope(prevDef: TypeDef) extends SemanticError
