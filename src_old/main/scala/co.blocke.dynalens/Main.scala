package co.blocke.dynalens

object Main:

  def main(args: Array[String]): Unit =
    println("=== Path.parsePath tests ===")

    val tests = Seq(
      "foo",
      "foo.bar",
      "foo[0]",
      "foo.bar",
      "foo.bar[3]",
      "foo.bar[3].baz",
      "outer.inner[foom].deep.value",
      "list[12]",
      "optList[0]",
      "nested.field[9].more"
    )

    tests.foreach { path =>
      val parsed = Path.parsePath(path)
      println(s"Input:  $path")
      println(s"Parsed: $parsed")
      println("-" * 60)
    }

    // Also test segmentAndIndex helper
    println("=== segmentAndIndex tests ===")
    val segTests = Seq("foo[3]", "foo", "bar[false]")
    segTests.foreach { seg =>
      val (name, idx) = Path.segmentAndIndex(seg)
      println(s"Segment: '$seg'  ->  name='$name', index=$idx")
    }

    println("=== partialPath tests ===")
    val example = List(
      Path.Field("outer"),
      Path.IndexedField("inner", Some("2")),
      Path.Field("deep"),
      Path.IndexedField("stuff", None)
    )
    println(s"Partial path for $example:")
    println(s"  ${Path.partialPath(example)}")