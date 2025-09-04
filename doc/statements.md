# Statement Reference

This document covers all supported **statements** in the DynaLens DSL. These include updates, declarations, blocks, and conditionals. All statements operate relative to the current input object (usually a Scala case class or Map structure).

---

## Update Statement

```
path = valueExpr
```

Assigns a value to a field or nested field within the current object.

### Examples:

```
name = "Alice"
age = age + 1
```

### Collection Mapping:

If the path includes `[]`, the assignment is mapped across each element of the collection:

```
orders[].qty => this * 2
```

This doubles the `qty` field of each `orders` element.

---

## Val Declaration

```
val identifier = valueExpr
```

Declares a local variable usable in subsequent expressions within the same scope. Variables are immutable.

### Examples:

```
val x = 5 * 2
val bonus = if salary > 100000 then 2000 else 1000
```

Val declarations **cannot** be reassigned.

---

## Conditional Statement

```
if booleanExpr then statement
if booleanExpr then statement else statement
```

Executes a statement conditionally. Supports both single-line and block statements.

### Examples:

```
if x > 10 then foo = "big"
if flag then count = count + 1 else count = 0
```

With blocks:

```
if status == "OK" then {
  retry = false
  attempts = 0
} else {
  retry = true
}
```

---

## Block Statement

```
{
  statement*
}
```

Groups multiple statements as a unit. Useful inside `if` or `map` expressions.

### Example:

```
{
  val tax = price * 0.1
  total = price + tax
}
```

---

## Map Statement (=>)

### Example:

```
things[].qty => this + 1
```

This applies the RHS expression to each `thing.qty` element.

This nomenclature works for Scala Map values too, but here `this` has constructed fields `key` and `value`:

```
m => {
  val x = this.value * 3
  (this.key, x) # last expression for => with Map must be (key,value), to replace the old one
}
````

---

## Nested Statements

All statement forms can be nested.

### Example:

```
if items.len() > 0 then {
  val discount = 0.95
  items[].price => this * discount
}
```

## Map-To and Map-From

This is a special-purpose mapping feature, implemented as an "update-in-place" statement, like =>.
The intention is to translate values from one lexicon to another. For example, let's imagine your Item class
contains a partNo field. You're working with a business partner, who recognizes the same Item but uses their own 
part number. `mapTo` and `mapFrom` provide convenient translation services.

```
items[].number.mapTo("testmap")
```

`testmap` is a label assigned to the mapping table. So where does this table come from? Anyplae you like! It
can come from JSON, a database, etc. How you read it in is up to you. You provide this table, with a label to the
lens this way:

```scala
val numbers = Map("abc" -> "p123", "xyz" -> "p456") // populate this Map as you will...
val withRegistry = (new BiMapRegistry()).register("testmap", BiMap.fromMap(numbers))
for {
  compiledScript <- Script.compile(script, lens)
  (result, _) <- a.run(compiledScript, inst, withRegistry)
} yield result
```

`mapFrom` works exactly the same way--just translating the values the other direction.

## Related Docs

- [Documentation Main](dyna_lens_documentation.md)
- [Grammar](grammar.md)
- [Functions](functions.md)
- [Paths](paths.md)
- [Statements](statements.md)
- [Errors](errors.md)
- [Types](types.md)

