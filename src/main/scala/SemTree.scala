class SemTree {}

trait SemExp {
  val t: TypeExp
}

case class SemErrorExp(error: SemanticError, pos: MExp) extends SemExp {
  override val t = ErrorType
}
