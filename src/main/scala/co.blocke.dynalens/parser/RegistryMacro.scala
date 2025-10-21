package co.blocke.dynalens
package parser


import scala.quoted.*


object RegistryMacro:

  inline def gen: Map[String, CompileFn[?]] = ${ genImpl }

  private def genImpl(using Quotes): Expr[Map[String, CompileFn[?]]] =
    import quotes.reflect.*

    val targetPkg = "co.blocke.dynalens.parser.cfn"
    val pkg = Symbol.requiredPackage(targetPkg)

    // Collect all `object` symbols that extend CompileFn
    val modules: List[Symbol] =
      pkg.declarations.collect {
        case m if m.isTerm && m.flags.is(Flags.Module) &&
          Ref(m).tpe <:< TypeRepr.of[CompileFn[?]] =>
          m
      }
//    report.info(s"[RegistryMacro] Found CompileFn objects: ${modules.map(_.fullName).mkString("\n")}")
    // TODO: Remove this line for full system--here only to force macro to run during stripped tests
    val _ = modules.foreach(_.fullName) // forces evaluation

    if modules.isEmpty then
      report.errorAndAbort(
        s"No CompileFn objects found in package '$targetPkg'."
      )

    // Build name -> object pairs
    val pairs: List[(Expr[(String, CompileFn[?])], String)] =
      modules.map { m =>
        val objExpr = Ref(m).asExprOf[CompileFn[?]]
        val nameSel = Select.unique(Ref(m), "name").asExprOf[String]

        val constName: String =
          nameSel.value.getOrElse(s"<non-const:${m.fullName}>")

        ('{ ($nameSel, $objExpr) }, constName)
      }

    // Duplicate check
    val dups = pairs.map(_._2).groupBy(identity).collect { case (k, vs) if vs.size > 1 => k }
    if dups.nonEmpty then
      report.errorAndAbort(s"Duplicate CompileFn.name detected: ${dups.mkString(", ")}")

    val listExpr: Expr[List[(String, CompileFn[?])]] =
      Expr.ofList(pairs.map(_._1))

    '{ $listExpr.toMap }