package co.blocke.dynalens


// Extract vars for interpolation
object TemplateUtils {
  private val varPattern =
    """\{([a-zA-Z_][a-zA-Z0-9_]*(?:\[[0-9]+\]|\.[a-zA-Z_][a-zA-Z0-9_]*(?:\[[0-9]+\])?)*)(?:%[^}:]+)?(?::[^}]+)?\}""".r

  def extractVariables(template: String): Set[String] =
    varPattern
      .findAllMatchIn(template)
      .flatMap { m =>
        Option(m.group(1)).orElse(Option(m.group(2)))
      }
      .toSet

  def fill(template: String, values: Map[String, Any]): String = {
    varPattern.replaceAllIn(template, m => {
      val key = m.group(1)
      values.get(key) match {
        case Some(null) | None => "" // Missing values become empty
        case Some(v) => v.toString
      }
    })
  }  
}
