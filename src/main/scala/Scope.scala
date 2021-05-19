import scala.collection.mutable

class Scope(
    val parent: Option[Scope] = Some(BuiltinScope)
) {
  val varDefs: EnvChain[String, VarDef] = new EnvChain(
    parent.map(parent => parent.varDefs)
  )
  val typeDefs: EnvChain[String, TypeDef] = new EnvChain(
    parent.map(parent => parent.typeDefs)
  )
}

class EnvChain[K, V](val parent: Option[EnvChain[K, V]] = None) {
  val env: mutable.Map[K, V] = mutable.Map[K, V]()
  def lookupDirect(name: K): Option[V] = env.get(name)
  def lookup(name: K): Option[V] =
    env
      .get(name)
      .orElse(parent match {
        case Some(envChain) => envChain.lookup(name)
        case None           => None
      })
  def set(name: K, v: V): Unit = env(name) = v
}

class VarDef(val name: String, val t: TypeExp, val external: Boolean = false)

class TypeDef(val name: String, val d: Option[TypeExp] = None)
