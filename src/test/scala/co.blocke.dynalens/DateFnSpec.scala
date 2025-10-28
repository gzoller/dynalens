package co.blocke.dynalens
package fn

import zio.*
import zio.test.*
import co.blocke.testkit.ZioTestKit.*


object DateFnSpec extends ZIOSpecDefault {

  case class Foo(dt: java.time.LocalDateTime, s: String)

  val foo = Foo(java.time.LocalDateTime.of(2025, 1, 1, 12, 0), "abc")
  val fooLens = DynaLens.into[Foo].topLens

  def ctx = DynaContext(Map("foo" -> (foo, fooLens)))

  def spec = suite("DateFnSpec")(
    suite("formatDate")(
      test("formats LocalDateTime successfully") {
        val fn = FormatDateFn(G("foo.dt"), C("yyyy-MM-dd"), "")
        for
          exit <- fn.resolve(ctx).exit
        yield assertTrue {
          exit match
            case Exit.Success((v: String, _)) =>
              v == "2025-01-01"
            case _ => false
        }
      },

      test("error: receiver not a date") {
        val fn = FormatDateFn(G("foo.s"), C("yyyy"), "")
        for exit <- fn.resolve(ctx).exit
          yield assertTrue(exit.isFailure)
      },

      test("error: null receiver") {
        val badCtx = DynaContext(Map("foo" -> (Foo(null.asInstanceOf[java.time.LocalDateTime], "abc"), fooLens)))
        val fn = FormatDateFn(G("foo.dt"), C("yyyy"), "")
        for exit <- fn.resolve(badCtx).exit
          yield assertTrue(exit.isFailure)
      }
    ).provide(
      ZLayer.succeed(RuntimeEnv(new BiMapRegistry()))
    ) @@ ziotestkit,

    suite("parseDate")(
      test("parse string → LocalDateTime successfully") {
        val fn = ParseDateFn(C("2025-01-01 08:30"), C("yyyy-MM-dd HH:mm"), "")
        for exit <- fn.resolve(ctx).exit
          yield assertTrue {
            exit match
              case Exit.Success((v: java.time.LocalDateTime, _)) =>
                v.getYear == 2025 && v.getHour == 8 && v.getMinute == 30
              case _ => false
          }
      },

      test("error: source not string") {
        val fn = ParseDateFn(G("foo.dt"), C("yyyy"), "")
        for exit <- fn.resolve(ctx).exit
          yield assertTrue(exit.isFailure)
      },

      test("error: null source") {
        val badFn = ParseDateFn(C(null), C("yyyy-MM-dd"), "")
        for exit <- badFn.resolve(ctx).exit
          yield assertTrue(exit.isFailure)
      }
    ).provide(
      ZLayer.succeed(RuntimeEnv(new BiMapRegistry()))
    ) @@ ziotestkit,

    suite("now()")(
      test("returns LocalDateTime with valid lens") {
        val fn = NowFn("")
        for exit <- fn.resolve(ctx).exit
          yield assertTrue {
            exit match
              case Exit.Success((v: java.time.LocalDateTime, lens)) =>
                lens.name == "now" && lens.parent.isEmpty
              case _ => false
          }
      }
    ).provide(
      ZLayer.succeed(RuntimeEnv(new BiMapRegistry()))
    ) @@ ziotestkit
  )
}