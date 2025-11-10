package co.blocke.dynalens
package parser

import co.blocke.dynalens.fn.{GetFn, RootFn, NoOpFn}

case class ExprContext(
                        scriptText: String,
                        schema: ClassType,
                        symbols: List[Map[String, FieldType]] = Nil,
                        receiver: Option[Receiver] = None,
                        pos: Int = 0
                      ) {

  def pushScope(fields: List[FieldType]): ExprContext =
    copy(symbols = fields.map(ft => ft.name -> ft).toMap :: symbols)

  def resolveSymbol(name: String): Option[FieldType] =
    symbols.collectFirst { case m if m.contains(name) => m(name) }

  def resolveSchemaFor(receiver: Fn[?]): FieldType =
    receiver match
      case RootFn | NoOpFn =>
        val res = this.receiver.map(_.ftype).getOrElse(schema)
        res

      case g: GetFn =>
        val baseSchema =
          if g.path.startsWith("this.") && this.receiver.nonEmpty then {
            this.receiver.get.ftype
          } else {
            val fromSymbols = symbols.iterator.flatMap(_.get(g.path)).toList.headOption
            fromSymbols.getOrElse(schema)
          }
        val finalType = util.PathUtil
          .getPathType(g.path, baseSchema)
          .getOrElse(ScalarType("", "scala.Any"))
        finalType

      case _ =>
        schema

  /** Add value bindings immutably. */
  def withVals(newVals: (String, FieldType)*): ExprContext =
    val updatedHead = symbols.headOption.getOrElse(Map.empty) ++ newVals
    val tail = if symbols.nonEmpty then symbols.tail else Nil
    copy(symbols = updatedHead :: tail)

  // For error messages
  val posStr: String = posStrFrom(pos)
  def posStrFrom(pos: Int): String =
    val lines = scriptText.take(pos).split('\n')
    val line  = lines.length
    val col   = lines.lastOption.map(_.length).getOrElse(0) + 1
    s"[$line,$col]"

  /** Install a new receiver from a given schema path. */
  def withReceiverFromPath(path: String): Either[DLCompileError, ExprContext] =
    val baseReceiver =
      if path.startsWith("this.") then
        receiver.map(_.fn).getOrElse(RootFn)
      else if path == "this_key" then
        receiver match
          case Some(NamedReceiver(_, mapType: MapType, fn)) =>
            GetFn(path, mapType.keyType.isOptional, fn, posStr)
          case _ =>
            RootFn
      else if path == "this_value" then
        receiver match
          case Some(NamedReceiver(_, mapType: MapType, fn)) =>
            GetFn(path, mapType.valueType.isOptional, fn, posStr)
          case _ =>
            RootFn
      else
        GetFn(path, false, RootFn, posStr)
  
    val baseSchema = resolveSchemaFor(baseReceiver)
  
    util.PathUtil.getPathType(path, baseSchema) match
      case Left(msg) =>
        Left(DLCompileError(posStr, msg))
      case Right(ft) =>
        val fn = GetFn(path, ft.isOptional, RootFn, posStr)
        Right(copy(receiver = Some(NamedReceiver(path, ft, fn))))        

  /** Directly set a receiver object. */
  def withReceiver(recv: Receiver): ExprContext =
    copy(receiver = Some(recv))

  /** Merge contexts across sequential statements without leaking receiver. */
  def merge(that: ExprContext): ExprContext =
    ExprContext(
      scriptText = this.scriptText,
      schema     = this.schema,
      symbols    = that.symbols ++ this.symbols,
      receiver   = this.receiver,
      pos        = this.pos
    )

  override def toString: String =
    val recvStr   = receiver.map(_.toString).getOrElse("None")
    val schemaStr = Option(schema).map(_.toString).getOrElse("None")
    val symsStr   = if symbols.isEmpty then "{}"
    else symbols.map(m => m.map { case (k,v) => s"$k -> $v" }.mkString("{", ", ", "}")).mkString("\n")
    s"""ExprContext(
       |  schema   = $schemaStr
       |  symbols  = $symsStr
       |  receiver = $recvStr
       |  pos      = $pos
       |)""".stripMargin
}