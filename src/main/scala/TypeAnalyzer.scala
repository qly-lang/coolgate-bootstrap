import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class SymbolTable(val rootScope: Scope, val scopes: Map[MExp, Scope])

class TypeAnalyzeResult(val symbolTable: SymbolTable, val errors: Vector[SemErrorExp])

class TypeAnalyzer(val ast: AST) {
  val rootScope = new Scope()
  val scopes: mutable.Map[MExp, Scope] = mutable.Map[MExp, Scope]()
  val errors: mutable.ArrayBuffer[SemErrorExp] = mutable.ArrayBuffer()

  def semError(error: SemanticError, mexp: MExp): ErrorType.type = {
    errors.append(SemErrorExp(error, mexp))
    ErrorType
  }

  def analyze = {
    analyzeTypeMExps(ast.mexps, rootScope)
    new TypeAnalyzeResult(new SymbolTable(rootScope, scopes.toMap), errors.toVector)
  }

  def analyzeTypeMExps(mexps: Seq[MExp], scope: Scope): Unit = {
    mexps.foreach(analyzeTypeMExpOut(_, scope))
    mexps.foreach(analyzeTypeMExpIn(_, scope))
  }

  def analyzeTypeMExpOut(mexp: MExp, scope: Scope): Unit = {
    mexp match {
      case OpExp(SymbolLit("v"), ColonExp(SymbolLit(variable), t) +: _) =>
        val ty = processType(t, scope)
        scope.varDefs.set(variable, new VarDef(variable, ty))
      case OpExp(SymbolLit("f"), SymbolLit(fname) +: parameters +: _) =>
        val t = parameters match {
          case ColonExp(ArrayExp(params), returnType) =>
            FunType(processParamTypes(params, scope), processType(returnType, scope))
          case _ =>
            semError(
              MalformedOp(
                "function parameters should be [param:type ...] or [param:type ...]:return-type"
              ),
              parameters
            )
        }
        scope.varDefs.set(fname, new VarDef(fname, t))
      case OpExp(SymbolLit("t"), args) => {
        args match {
          case Vector(t: SymbolLit, typeDef) =>
            // t[type1 typeDef]
            val d = scope.typeDefs.lookupDirect(t.value)
            if (d.isDefined) { throw TypeAlreadyDefinedInScope(d.get) }
            val de = processType(typeDef, scope)
            val td = new TypeDef(t.value, d = Some(de))
            scope.typeDefs.set(t.value, td)
        }
      }
    }
  }

  def analyzeTypeMExpIn(mexp: MExp, scope: Scope) = {}

  def processType(t: MExp, scope: Scope): TypeExp = {
    t match {
      case SymbolLit(name) =>
        val referTo = scope.typeDefs.lookup(name)
        referTo match {
          case Some(to) => Refer(to)
          case None     => semError(UndefinedType, t)
        }
      case OpExp(SymbolLit("array"), args) =>
        if (args.length != 2) { return semError(MalformedType("Expect elem-type and len"), t) }
        val elemType = processType(args(0), scope)
        val len = args(1) match {
          case IntLit(l) =>
            if (l <= 0) {
              return semError(MalformedType("Array length should be greater than 0"), args(1))
            }
            l
          case _ => return semError(MalformedType("length must be integer const"), args(1))
        }
        ArrayType(elemType, len)
      case OpExp(SymbolLit("struct"), args) =>
        if (args.exists(elem => elem.getClass != classOf[ColonExp])) {
          return semError(MalformedType("struct[] fields should be colon exps"), t)
        }
        val fields = args.map(arg => processFieldType(arg.asInstanceOf[ColonExp], scope))
        try { StructType(fields.map(_.get)) }
        catch { case _ => semError(MalformedType("Some struct field malformed"), t) }
      case OpExp(SymbolLit("union"), args) =>
        if (args.exists(elem => elem.getClass != classOf[ColonExp])) {
          return semError(MalformedType("union[] fields should be colon exps"), t)
        }
        val fields = args.map(arg => processFieldType(arg.asInstanceOf[ColonExp], scope))
        try { UnionType(fields.map(_.get)) }
        catch { case _ => semError(MalformedType("Some union field malformed"), t) }
      case OpExp(SymbolLit("f"), Vector(ColonExp(ArrayExp(types), returnType))) =>
        FunType(types.map(t => processType(t, scope)), processType(returnType, scope))
      case _ => semError(MalformedType("Unknown pattern of type"), t)
    }
  }

  def processFieldType(colonExp: ColonExp, scope: Scope): Try[Field] = {
    if (colonExp.exp.getClass != classOf[SymbolLit]) {
      Failure(semError(MalformedType("Field name need to be symbol"), colonExp))
    }
    Success(Field(colonExp.exp.asInstanceOf[SymbolLit].value, processType(colonExp.col, scope)))
  }

  def processParamTypes(params: Vector[MExp], scope: Scope): Vector[TypeExp] = {
    params.map(param => {
      if (param.getClass == classOf[ColonExp]) {
        processType(param.asInstanceOf[ColonExp].col, scope)
      } else { semError(MalformedOp("expect param:type in param lists"), param) }
    })
  }
}
