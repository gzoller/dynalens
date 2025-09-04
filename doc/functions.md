# DynaLens DSL Function Reference

This document details all built-in functions in the DynaLens DSL. All functions are invoked using **method-style syntax**:

```
valueExpr.functionName(arg1, arg2, ...)
```

---

## String Functions

| Function                | Description                                      | Example                            |
|-------------------------|--------------------------------------------------|------------------------------------|
| `toUpperCase()`         | Converts to uppercase                            | `name.toUpperCase()`               |
| `toLowerCase()`         | Converts to lowercase                            | `name.toLowerCase()`               |
| `trim()`                | Removes surrounding whitespace                   | `name.trim()`                      |
| `startsWith(str)`       | Checks if string starts with `str`               | `name.startsWith("Mr")`            |
| `endsWith(str)`         | Checks if string ends with `str`                 | `name.endsWith("son")`             |
| `contains(str)`         | Checks if string contains `str`                  | `msg.contains("error")`            |
| `equalsIgnoreCase(str)` | Case-insensitive equality comparison             | `foo.equalsIgnoreCase("bar")`      |
| `matchesRegex(regex)`   | True if string matches regex                     | `email.matchesRegex("[^@]+@.*")`   |
| `len()`                 | Length of the string                             | `name.len()`                       |
| `substr(start, end)`    | Substring from `start` to `end` (exclusive)      | `code.substr(0, 3)`                |
| `replace(old, new)`     | Replace substring `old` with `new`               | `msg.replace("ERR", "WARN")`       |
| `template(str)`         | Templated string with `${}` interpolation        | `"Hello, ${name}".template()`      |

---

## Math & Numeric Functions

| Function        | Description                              | Example                    |
|------------------|------------------------------------------|----------------------------|
| `abs()`          | Absolute value                           | `amount.abs()`             |
| `min(value)`     | Minimum of this and another value        | `score.min(50)`            |
| `max(value)`     | Maximum of this and another value        | `price.max(100)`           |
| `sum()`          | Sum of values in list                    | `items[].qty.sum()`        |
| `avg()`          | Average of values in list                | `temps.avg()`              |
| `median()`       | Median of values in list                 | `ratings.median()`         |

---

## Boolean / Option Functions

| Function      | Description                                  | Example                     |
|---------------|----------------------------------------------|-----------------------------|
| `isDefined()` | True if the field is present (not null/None) | `email.isDefined()`         |
| `else(value)` | Fallback value if optional field is missing  | `zipCode.else("00000")`     |
| `!<boolExpr>` | Logical NOT (unary)                          | `!hasErrors`                |

---

## Collection Functions

| Function           | Description                                  | Example                             |
|--------------------|----------------------------------------------|-------------------------------------|
| `len()`            | Number of elements in list or map            | `items.len()`                       |
| `filter(expr)`     | Filter elements by boolean expression        | `items.filter(x.qty > 0)`           |
| `sortAsc()`        | Sort list ascending                          | `prices.sortAsc()`                  |
| `sortDesc()`       | Sort list descending                         | `grades.sortDesc()`                 |
| `distinct()`       | Remove duplicate values                      | `tags.distinct()`                   |
| `limit(n)`         | Keep only the first `n` elements             | `logs.limit(100)`                   |
| `reverse()`        | Reverse the list                             | `results.reverse()`                 |
| `clean()`          | Remove null/undefined entries                | `values.clean()`                    |

> `x` refers to each item during iteration (e.g., `items.map(x => x.qty)`).

---

## Map Functions

| Function     | Description                        | Example                          |
|--------------|------------------------------------|----------------------------------|
| `keys()`     | Returns list of keys               | `myMap.keys()`                   |
| `values()`   | Returns list of values             | `myMap.values()`                 |
| `get(key)`   | Retrieve value by key              | `myMap.get("total")`             |

---

## Mapping Functions

| Function       | Description                                   | Example                          |
|----------------|-----------------------------------------------|----------------------------------|
| `mapFrom(fn)`  | Transform value from external to internal     | `items[].id.mapFrom(mySchema)`   |
| `mapTo(fn)`    | Reverse transform from internal to external   | `items[].id.mapTo(outputSchema)` |

> These may be **disabled** in some runtime modes.

---

## Date / Utility 

| Function       | Description                                  | Example                      |
|----------------|----------------------------------------------|------------------------------|
| `dateFmt(fmt)` | Format a date string using a format          | `birthdate.dateFmt("yyyy-MM-dd")` |
| `toDate(fmt?)` | Parse string to date (optional input format) | `"2024-08-02".toDate()`           |
| `now()`        | Current timestamp (if supported)             | `now().dateFmt("HH:mm:ss")`  |
| `uuid()`       | Generate UUID (if supported)                 | `uuid()`                     |

---

## Case Function (Inline Match)

```
field.case(
  "A" -> "one",
  "B" -> "two",
  default -> "other"
)
```

- Match-like function returning values by key.
- Scalar keys only (string, int, bool).
- Supports optional `default`.

Permissive version:

```
field.case(permissive)(
  "A" -> "x",
  "B" -> "y"
)
```

- If input doesn't match and no `default`, returns original value (pass-through).

---

## Custom and Chained Logic

| Pattern                   | Description                          | Example                              |
|---------------------------|--------------------------------------|--------------------------------------|
| `val` constants           | Reuse values in expressions          | `val big = qty > 100`                |
| Method chaining           | Pipe function calls                  | `name.trim().toLowerCase().len()`    |
| Conditional assignments   | Use with `if (cond) { ... } else {}` | `if (age > 18) { group = "adult" }`  |

---

## Related Docs

- [Documentation Main](dyna_lens_documentation.md)
- [Grammar](grammar.md)
- [Functions](functions.md)
- [Paths](paths.md)
- [Statements](statements.md)
- [Errors](errors.md)
- [Types](types.md)


---