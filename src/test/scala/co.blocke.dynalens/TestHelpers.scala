package co.blocke.dynalens

import zio.ZIO


object TestHelpers:

  /** Convenience builder for hand-rolled ClassLens in tests */
  def classLensFor[T](
                       name: String,
                       fields: Map[String, Lens],
                       getFn: (String, Any) => ZIO[Any, DynaLensError, Any],
                       updFn: (String, Any, Any) => ZIO[Any, DynaLensError, Any],
                       schema: ClassType
                     ): ClassLens =
    ClassLens(
      name = name,
      isOptional = false,
      parent = None,
      fields = fields,
      _get = getFn,
      _update = updFn,
      schema = schema
    )


  val emptyCL =
    ClassLens(
      name = "noop",
      isOptional = false,
      parent = None,
      fields = Map.empty,
      _get = (_, obj) => ZIO.succeed(obj),
      _update = (_, _, _) => ZIO.succeed(()),
      schema = ClassType("", "java.lang.Object", Nil)
    )

  val emptyDL =
    DynaLens(
      topLens = emptyCL,
      schema = ClassType("", "java.lang.Object", Nil),
      registry = Map.empty
    )