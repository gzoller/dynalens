# 📌 DynaLens DSL Cheatsheet

A quick reference guide to the syntax, paths, functions, and runtime behavior of the DynaLens dynamic transformation language.

---

## Statements

| Syntax                         | Description                            |
| ------------------------------ | -------------------------------------- |
| `val x = expr`                 | Declare a named constant               |
| `field = expr`                 | Assign value to a field                |
| `field => source case { ... }` | Match `source` and map to `field`      |
| `if (cond) { ... } else {}`    | Conditional logic                      |
| `{ stmt1 stmt2 ... }`          | Statement block                        |

---

## Paths

| Form                  | Description                                  |
| --------------------- |----------------------------------------------|
| `foo.bar`             | Nested field access                          |
| `foo[0]`              | Index into list                              |
| `foo.bar`             | Path into list of objects (if foo is a list) |
| `foo.bar.else("x")`   | Default if optional is missing               |
| `foo.bar.isDefined()` | Boolean check for optional value             |
| `foo.bar.len()`       | Length of string, list, or map               |

---

## Core Functions

### Math

| Function            | Description             |
|---------------------|-------------------------|
| `+`, `-`, `*`, `/`  | Arithmetic operations   |
| `min(a,b)`          | Minimum of two values   |
| `max(a,b)`          | Maximum of two values   |
| `sum(list)`         | Sum of list values      |
| `avg(list)`         | Average of list values  |
| `median(list)`      | Median value            |
| `abs(x)`            | Absolute value          |

### String

| Function                | Description                       |
|-------------------------|-----------------------------------|
| `::`                    | String concatenation              |
| `startsWith(str)`       | Prefix match                      |
| `endsWith(str)`         | Suffix match                      |
| `contains(substr)`      | Substring presence                |
| `equalsIgnoreCase(str)` | Case-insensitive equality         |
| `matchesRegex(regex)`   | Regex pattern match               |
| `toUpperCase()`         | Convert to upper case             |
| `toLowerCase()`         | Convert to lower case             |
| `trim()`                | Strip leading/trailing whitespace |
| `substr(start, end)`    | Substring from start to end       |
| `replace(find, repl)`   | Replace text                      |
| `len()`                 | Length of string                  |
| `template(str)`         | Interpolation-based templating    |
| `dateFmt(fmt)`          | Format date string                |
| `toDate(fmt?)`          | Parse string to date              |

### Collection (Lists & Maps)

| Function          | Description                                    |
|-------------------|------------------------------------------------|
| `len()`           | Length of collection                           |
| `sortAsc()`       | Sort ascending                                 |
| `sortDesc()`      | Sort descending                                |
| `distinct()`      | Remove duplicates                              |
| `distinct(field)` | Remove duplicates (list of class, using field) |
| `filter(fn)`      | Keep elements matching condition               |
| `limit(n)`        | Keep only first `n` items                      |
| `reverse()`       | Reverse order                                  |
| `clean()`         | Remove null/undefined entries                  |

### Map-Specific

| Function     | Description                  |
|--------------|------------------------------|
| `keys()`     | List of keys                 |
| `values()`   | List of values               |
| `get(key)`   | Lookup by key                |

### Mapping Functions

| Function       | Description                                 |
|----------------|---------------------------------------------|
| `mapFrom(fn)`  | Transform from one schema to another        |
| `mapTo(fn)`    | Reverse transformation                      |

> ⚠️ `mapFrom` and `mapTo` may be disabled in some builds.

### Date & Miscellaneous

| Function   | Description       |
|------------|-------------------|
| `now()`    | Current timestamp |
| `uuid()`   | Generate a UUID   |

---

## Optionals & Safety

| Function         | Description                          |
|------------------|--------------------------------------|
| `isDefined()`    | Returns true if field has value      |
| `else(x)`        | Fallback value if `Option` is empty  |

---

## Boolean Logic

| Syntax / Function     | Description                         |
|------------------------|------------------------------------|
| `==`, `!=`, `<`, `>`, `<=`, `>=` | Comparisons              |
| `!a`                  | Not                                 |
| `a && b`              | Logical and                         |
| `a \|\| b`            | Logical or                          |

---

## Case Mapping

```
category => item.type case {
  "A"     -> "Item-A"
  "B"     -> "Item-B"
  default -> "Unknown"
}
```

- Supports optional `default` clause.
- Match values must be literals.
- RHS expressions can be anything valid.
- Optional `(permissive)` mode allows fallback without error.
```
# Will pass through any values that do not match
category => item.type case (permissive) {
  "A"     -> "Item-A"
  "B"     -> "Item-B"
}
```

---

## Conditionals

```
if (qty > 10) 
  tier = "bulk"
else 
  tier = "standard"
```

> Multi-line bodies are suppored using standard '{}' notation

---

## Types

| Type         | Notes                                    |
|--------------|------------------------------------------|
| `String`     | Text values                              |
| `Int`        | Whole numbers                            |
| `Double`     | Decimal numbers                          |
| `Boolean`    | true / false                             |
| `List[A]`    | Lists of values                          |
| `Map[K,V]`   | Key-value map                            |
| `Option[A]`  | Nullable types                           |

---

## Error Types

| Error Type        | Trigger                         |
|-------------------|---------------------------------|
| `DynaLensError`   | Error during script execution   |
| `DLCompileError`  | Error during script compilation |

---

## Tips

- Use `.else("fallback")` for safe `Option` access.
- Chains like `foo.bar.len()` are valid.
- You can `val`-assign intermediate values.
- `filter()` supports functional lambdas as an argument.
- Avoid using `==` on `Option` types directly—use `isDefined()`.

---

## Related Docs

- [Documentation Main](dyna_lens_documentation.md)
- [Grammar](grammar.md)
- [Functions](functions.md)
- [Paths](paths.md)
- [Statements](statements.md)
- [Errors](errors.md)
- [Types](types.md)