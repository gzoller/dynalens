# Error Handling in DynaLens

This document outlines how the DynaLens DSL handles errors at **compile time** and **runtime**. It includes common error types, how they arise, and strategies to handle or avoid them.

---

## Compile-Time Errors

Compile-time errors are thrown during DSL parsing/validation before runtime execution. They are returned as `DLCompileError`.

### Examples:

- **Invalid path syntax**:

  ```
  foo..bar   // ERROR: double dot not allowed
  ```

- **Invalid case pattern**:

  ```
  foo case { other -> 1 }  // ERROR: 'other' must be literal
  ```

- **Reserved word misuse**:

  ```
  val default = 3   // ERROR: 'default' is reserved in case{}
  ```

---

## Runtime Errors

Runtime errors occur during execution of a compiled DynaLens program. These are surfaced as `DynaLensError` and wrapped in a ZIO failure.

### Common Runtime Errors

| Type                     | Description                                                                |
| ------------------------ | -------------------------------------------------------------------------- |
| `MissingFieldError`      | Accessing a field that doesn't exist or is null                            |
| `InvalidPathError`       | Trying to index into a non-collection field                                |
| `FunctionArgError`       | Calling a function with the wrong number or type of arguments              |
| `UnmatchedCaseError`     | No match in `case {}` and not in permissive mode                           |
| `UnsupportedOperation`   | Using a function or operator on an incompatible type                       |
| `CircularReferenceError` | A path expression refers to itself in a way that causes recursion or loops |

### Example

```
m => (this.key, this.value case { 1 -> 7, 2 -> 19 })
```

Given:

```
Mapped(1, m = Map("a" -> 1, "b" -> 2, "c" -> 3))
```

If `this.value = 3`, this causes:

```text
DynaLensError: No case matched for value: 3
```

---

## Defensive Coding Practices

- Use `.else(...)` to provide fallbacks for optional fields:

  ```
  contact.email.else("no-email")
  ```

- Use `case (permissive)` mode to tolerate unmatched values:

  ```
  value case(permissive) { 1 -> "one", 2 -> "two" }
  ```

- Validate whether optional values exist with `isDefined()`:

  ```
  address.zip.isDefined()
  ```

---

## Related Docs

- [Documentation Main](dyna_lens_documentation.md)
- [Grammar](grammar.md)
- [Functions](functions.md)
- [Paths](paths.md)
- [Statements](statements.md)
- [Errors](errors.md)
- [Types](types.md)