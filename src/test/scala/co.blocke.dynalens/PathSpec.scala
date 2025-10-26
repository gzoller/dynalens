package co.blocke.dynalens

import zio.*
import zio.test.*

object PathSpec extends ZIOSpecDefault:

  def spec = suite("Path Parser Tests")(

    test("Parse single segment path") {
      val path = "order"
      val parsed = Path.parsePath(path)
      assertTrue(
        parsed == List(PathElement(Some("order"), None))
      )
    },

    test("Parse multi-level path with index") {
      val path = "order.items[3].desc"
      val parsed = Path.parsePath(path)
      assertTrue(
        parsed == List(
          PathElement(Some("order"), None),
          PathElement(Some("items"), Some("3")),
          PathElement(Some("desc"), None)
        )
      )
    },

    test("Parse map-style index") {
      val path = "settings[theme]"
      val parsed = Path.parsePath(path)
      assertTrue(
        parsed == List(PathElement(Some("settings"), Some("theme")))
      )
    },
    test("Parse nested list/map chain") {
      val path = "holder.items[0][b].desc"
      val parsed = Path.parsePath(path)
      assertTrue(
        parsed == List(
          PathElement(Some("holder"), None),
          PathElement(Some("items"), Some("0")),
          PathElement(None, Some("b")),
          PathElement(Some("desc"), None)
        )
      )
    },
    test("Ignore malformed segments") {
      val path = "order..items[].desc"
      val parsed = Path.parsePath(path)
      // Should skip malformed ones but still parse valid parts
      assertTrue(
        parsed == List(
          PathElement(Some("order"), None),
          PathElement(Some("desc"), None)
        )
      )
    }
  )
