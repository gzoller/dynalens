# Type System in DynaLens

DynaLens uses a dynamically typed runtime model, but type awareness is embedded into function resolution, path evaluation, and error handling. This document explains how types are inferred, propagated, and validated.

---

## Primitive Types

These are the core runtime types that DynaLens recognizes:

* Int
* Long
* Float
* Double
* String
* Boolean

---

## Collection Types

| Type             | Description                    |
| ---------------- | ------------------------------ |
| `List[A]`        | Ordered list of items          |
| `Map[String, A]` | Key-value map with string keys |

Collections support iteration (`[]`), mapping (`map`), and indexing. Map keys must be scalar (String, Int, Long, etc.).

### Example:

```
products.price = this * 1.2
```

---

## Option Types

Optional fields are modeled as `Option[T]` (i.e., possibly missing).

- Use `.isDefined()` to test
- Use `.else()` to supply a fallback

### Example:

```
val userPhone = contact.phone.else("unknown")
```

If `phone` is not defined, it evaluates to "unknown". Data type for else() should match the type of the Option element type.

---

## Type Inference

- Types are inferred at runtime based on the input object
- Function application performs dynamic type checks
- Mismatches yield `DynaLensError`

### Examples of Safe vs Unsafe:

```
x = 5 + 3         // OK
x = "foo" + 3     // ERROR: String + Int not allowed
```

---

## Function Type Signatures

Functions have enforced argument types:

| Function       | Signature                        | Notes       |
| -------------- | -------------------------------- | ----------- |
| `len()`        | `List[_] => Int`                 | List length |
| `startsWith()` | `String => Boolean`              |             |
| `map()`        | `List[A], Fn[A] => B => List[B]` |             |

See [`functions.md`](functions.md) for the full list.

---

## Type Conversion

Explicit conversion is limited; most coercion must be manual:

```
id = id.toString()
```

Avoid relying on implicit coercion. Type mismatches will cause runtime errors.

---

## Error Cases

| Situation                    | Example                     | Outcome                |
| ---------------------------- | --------------------------- | ---------------------- |
| Wrong function argument type | `toUpper(42)`               | `FunctionArgError`     |
| Add string + number          | `"foo" + 1`                 | `UnsupportedOperation` |
| Index into non-list          | `foo[0]` where `foo` is Int | `InvalidPathError`     |

---

## Related Docs

- [Documentation Main](dyna_lens_documentation.md)
- [Grammar](grammar.md)
- [Functions](functions.md)
- [Paths](paths.md)
- [Statements](statements.md)
- [Errors](errors.md)
- [Types](types.md)
