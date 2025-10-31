package co.blocke.dynalens
package parser


sealed trait Receiver:
  def ftype: FieldType

  def fn: Fn[Any]

  def pathName: Option[String]

  def elementTypeOpt: Option[FieldType]

  def lookupMethod(name: String)(using ctx: ExprContext): Either[DLCompileError, CompileFn] =
    CompileFnRegistry.lookup(name) match
      case None =>
        Left(DLCompileError(ctx.posStr, s"Method '$name' unknown"))
      case Some(cfn) =>
        if !cfn.accepts(this)(using ctx) then
          Left(DLCompileError(ctx.posStr, s"Method '$name' cannot be applied to receiver of type ${ftype.typeName}"))
        else
          Right(cfn)


case class NamedReceiver(path: String, ftype: FieldType, fn: Fn[Any]) extends Receiver:
  def pathName: Option[String] = Some(path)
  def elementTypeOpt: Option[FieldType] = ftype match
    case lt: ListType => Some(lt.elementType)
    case mt: MapType => Some(mt.valueType)
    case _ => None


case class ConstantReceiver(ftype: FieldType, fn: Fn[Any]) extends Receiver:
  def pathName: Option[String] = None
  def elementTypeOpt: Option[FieldType] = None


case class MethodReceiver(
                           parent: Receiver,
                           ftype: FieldType,
                           fn: Fn[Any]
                         ) extends Receiver:
  def pathName: Option[String] =
    parent.pathName.map(p => s"$p.<method>") // purely cosmetic, can omit if undesired

  def elementTypeOpt: Option[FieldType] = ftype match
    case lt: ListType => Some(lt.elementType)
    case mt: MapType  => Some(mt.valueType)
    case _            => None

  override def lookupMethod(name: String)(using ctx: ExprContext)
  : Either[DLCompileError, CompileFn] =
    CompileFnRegistry.lookup(name) match
      case None => Left(DLCompileError(ctx.posStr, s"Unknown method '$name'"))
      case Some(cfn) =>
        if cfn.accepts(this)(using ctx) then Right(cfn)
        else Left(DLCompileError(ctx.posStr,
          s"Method '$name' not applicable to type ${ftype.typeName}"))


case object SystemReceiver extends Receiver:
  def ftype: FieldType = ScalarType("AnyRoot", "scala.Any")
  def fn: Fn[Any] = co.blocke.dynalens.fn.NoOpFn //throw new IllegalStateException("SystemReceiver has no runtime Fn")
  def pathName: Option[String] = None
  def elementTypeOpt: Option[FieldType] = None


case class ElementReceiver(parent: Receiver, ftype: FieldType) extends Receiver:
  def fn: Fn[Any] = parent.fn
  def pathName: Option[String] = parent.pathName
  def elementTypeOpt: Option[FieldType] = ftype match
    case lt: ListType => Some(lt.elementType)
    case mt: MapType  => Some(mt.valueType)
    case _            => None