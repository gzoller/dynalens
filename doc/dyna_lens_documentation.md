# DynaLens DSL Language Reference

Welcome to the **DynaLens DSL** documentation. This is the complete reference for the DynaLens runtime scripting language, used to query, update, and transform arbitrary object graphs at runtime using a rich expression syntax.

---

## Overview

DynaLens is a domain-specific language (DSL) that enables:

- **Declarative updates** to object structures (JSON-like data)
- **Mapping and filtering** collections
- **Control flow** like `if` statements
- **Function composition** and fallback logic
- **Strict optionality enforcement** (unless permissive mode is enabled)

All expressions resolve against a root object, and runtime functions are applied dynamically.

---

## Key Features

-  **Path-based access** to deeply nested fields
-  **Iterative updates** via collection traversal
- ⚙ **Rich function library** for logic, math, string, date, and list ops
-  **Context merging** and scoping for map/filter
-  **Strict optional handling** with `.else()`
-  **Test-friendly structure** for embedded evaluations

---

##  Example Script

```
val bonus = if emp.salary > 100000 then 2000 else 1000

emp.performanceScore = 95

orders.qty => qty * 2

products.desc = name.toUpper() :: " (INVENTORY ITEM)"
```

---

## Documentation Index

- [Documentation Main](dyna_lens_documentation.md)
- [Grammar](grammar.md)
- [Functions](functions.md)
- [Paths](paths.md)
- [Statements](statements.md)
- [Errors](errors.md)
- [Types](types.md)