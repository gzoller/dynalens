# Grammar Syntax

This document describes the complete grammar of the DynaLens DSL in modular FastParse-style notation. Each parser is defined using combinators with left-to-right precedence and optional whitespace handling. All paths and expressions resolve relative to a given runtime object.

---

## Top-Level Structure

```
script         ::= statement*
statement      ::= updateStmt | valDecl | ifStmt | blockStmt
```

A DynaLens script is a series of top-level statements. Statements include:

- Assignments to paths (`foo.bar = 42`)
- `val` declarations
- Conditional blocks
- Nested block statements (`{ ... }`)

---

## Update Statement

```
updateStmt     ::= path '=' valueExpr
```

An update statement modifies a field or collection element. If the path contains `[]`, it performs an implicit `map` over the collection:

```
field = 10                 // simple field assignment
orders[].qty => this * 2     // map over orders, update each qty
```

---

## Update-in-Place (map) vs Assignment

DynaLens supports update-in-place capability. It treats your base object (the basis of the lens) as a mutable object,
even if it isn't delared that way. It functions like a Scala map() function. It's denoted by the "=>" operator.

```
items.description => "[verified]" :: this
```
This will prepend "[verified]" to the beginning of each item's description field value. In is a statement,
meaning it doesn't return a value. The following is illegal:

```
val x = items.description => "[verified]" :: this
```

Assignment is a 1-to-1 value assignment. For example: 
```
items[2].description = "[unverified]"
```

Using the '=' operator as a statement signifies assignment. "=>" is a looping construct under the covers.

Implicit nesting during a map operation is supported:

```
items[].subassem[].weight => this * 0.78
```
This effectively creates 2 nested loops for the map operation. Remember: the "[]" in the script is for your
benefit and clarity so you can see which path parts are seq and which are not. DynaLens doesn't need it, and you
may omit them:

```
items.subassem.weight => this * 0.78
```

---

## Val Declaration

```
valDecl        ::= 'val' identifier '=' valueExpr
```

Declares a named value in the current scope. May reference earlier bindings.

```
val bonus = if salary > 100000 then 2000 else 1000
```

---

## Conditional

```
ifStmt         ::= 'if' booleanExpr 'then' statement ('else' statement)?
```

Conditional execution of a statement.

```
if x > 10 then foo = 1 else foo = 2
```

---

## Block Statement

```
blockStmt      ::= '{' statement* '}'
```

Groups multiple statements as a unit. Useful inside `if` blocks.

```
if status == "OK" then {
  flag = true
  attempts = 0
}
```

---

## Path Expression

```
path           ::= identifier ('.' identifier | '[]')*
```

Used for accessing or modifying nested fields. `[]` indicates iteration over a collection.

```
name
address.street
items[].price
```
Note: '[]' notation is optional for clarity. DynaLens knows what fields are lists.

---

## Value Expressions

```
valueExpr      ::= logicExpr ('::' logicExpr)*     // string concat
```

```
logicExpr      ::= equalityExpr ('&&' equalityExpr)*
equalityExpr   ::= relationalExpr (('==' | '!=') relationalExpr)*
relationalExpr ::= additiveExpr (('>' | '>=' | '<' | '<=') additiveExpr)*
additiveExpr   ::= multiplicativeExpr ('+' | '-')*
multiplicativeExpr ::= unaryExpr ('*' | '/')*
unaryExpr       ::= '!' unaryExpr | primaryExpr
```

All standard operator precedences are supported.

---

## Primary Expressions

```
primaryExpr    ::= literal
                 | path
                 | '(' valueExpr ')'
                 | valueExpr '.' functionCall
```

Function calls can be chained to any value.

```
"foo".toUpper() # string
name.toLower().startsWith("a")  # boolean
```

---

## Literals

```
literal        ::= stringLiteral | numberLiteral | booleanLiteral
stringLiteral  ::= '"' .* '"'
numberLiteral  ::= [0-9]+ ('.' [0-9]+)?
booleanLiteral ::= 'true' | 'false'
```

---

## Function Calls

```
functionCall   ::= identifier '(' args? ')'
args           ::= valueExpr (',' valueExpr)*
```

Example:

```
name.startsWith("abc")
```
Functions may be chained if the types align.

---

## Match/Case Expression

```
matchExpr ::= valueExpr '=>' 'this' 'case' '{' matchCase+ '}'
matchCase ::= scalar '->' valueExpr
```

Example (planned):

```
status => this case {
  "ok" -> "pass"
  "fail" -> "retry"
}
```
This example combines a match case with a map over a list (status). Equivalent of saying: iterate over status and change "ok" to "pass" and "fail" to "retry".

---

## Related Docs

- [Documentation Main](dyna_lens_documentation.md)
- [Grammar](grammar.md)
- [Functions](functions.md)
- [Paths](paths.md)
- [Statements](statements.md)
- [Errors](errors.md)
- [Types](types.md)