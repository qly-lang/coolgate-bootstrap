object BuiltinScope extends Scope(parent = None) {
  def addType(name: String) = {
    typeDefs.set(name, new TypeDef(name))
  }

  val types = Vector(
    "no-value",
    "no-return",
    "intm",
    "uintm",
    "int8",
    "int16",
    "int32",
    "int64",
    "uint8",
    "uint16",
    "uint32",
    "uint64",
    "addr",
    "float32",
    "float64"
  )

  types.foreach(addType)

}
