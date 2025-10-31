package co.blocke.dynalens
package parser


object CompileFnRegistry:
  // materialized at compile time by the macro
  val functions: Map[String, CompileFn] = Map.empty //RegistryMacro.gen.  TODO

  // detect dupes at startup
  private val dupes = functions
    .groupBy(_._1)
    .filter(_._2.size > 1)

  if dupes.nonEmpty then
    val msg = dupes.map { case (n, entries) =>
      s"$n -> ${entries.map(_._2.getClass.getName).mkString(", ")}"
    }.mkString("\n")
    throw new IllegalStateException(s"Duplicate CompileFns registered:\n$msg")

  def lookup(name: String): Option[CompileFn] =
    functions.get(name)