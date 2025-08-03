


## dynalens Function Reference

#### Notes:
* Functions in dynalens always have () notation, even if there are no parameters specified.
* Functions associated with strings can apply to string literals (shown below), string-valued val's, or paths, for example foo.bar.contains("ok") if foo.bar is a string-valued field
* Boolean functions are intended to be used in comparitors, for example in an 'if' statement
* Most functions can be chained together: a().b()

|Function  | Description | Example | 
|--|--|--|
|startsWith  | Boolean-string starts with value | "foobar".startsWith("foo") |
|endsWith  | Boolean-string starts with value | "foobar".endsWith("bar") |
|contains  | Boolean-string contains value | "foobar".contains("oo") |
|equalsIgnoreCase  | Boolean-string compare ignoring case | "FooBar".equalsIgnoreCase("foobar")|
|matchesRegex  | Boolean-string matches regex | "FooBar".matchesRegex("F.*?B.*") | 
|toUpperCase  | Convert string to upper case | "foobar".toUpperCase() | 
|toLowerCase  | Covert string to lower case | "FooBar.toLowerCase() |
|template  | Populate a template string value<br>(string interpolation) | "Hello {name}, your price is {ticket.price}".template()<br>*(values in {} are paths in default context, ie fields in<br>top-level object)* |
|trim  | Remove leading/trailing whitespace from a string | "  FooBar ".trim() |
|substr  | Extract substring from string | "FooBar".substr(3) # Bar<br>"FooBar".substr(1,3) # oo |
|replace  | Replace a matched string with another | "YooHoo".repalce("oo","aa") # YaaHaa |
|:: | String concatenation<br>non-string values converted to string | "a" :: 4 :: "c" |
|len  | String or Iterable length | "Hello".len() # 5<br>items[].len() # num of items in list |
|uuid|Produce a random UUID|val id = uuid()|  
|now|Produce current date|val rightNow = now()|  
|dateFmt|Format a date per given Java date fmt |val old = when.dateFmt("MM-dd-yy")|  
|toDate|Parse a string to date w/format|"07-09-1972".toDate("MM-dd-yyyy")|  
|isDefined| Returns whether an optional field is defined| foo.bar.isDefined()

#### Collection (eg List) Methods
If a path points to a collection of something then dynalens provides a set of functions specific for changing collection values. Note that **these methods do not return a value!** They modify the collection given by the path in-place.

|Function  | Description | Example | 
|--|--|--|
|filter  | Filter an Interable, keeping items matching<br>predicate<br>*Note: Like an assignment, filter changes the<br>object in context. | items[].filter(this.price < 10.50) | 
|sortAsc  | Sort a list ascending | items[].sortAsc( price ) |
|sortDecs  | Sort a list descending | items[].sortDesc( price ) |
|distinct  | Remove duplicates on given field | items[].distinct( itemNum ) |
|limit  | Take the first 'n' items in the list, toss the rest | items[].limit(3) |
|mapTo  | String value mapping | (see Mapping below) |
|mapFrom  | String value mapping | (see Mapping below) |  
|reverse  | Reverse the order of a list | items[].reverse() |
|clean  | Remove any null, None, or unassigned (Unit) values in the list | items[].clean() |

#### Mapping
There are 2 special functions designed to do 2-way value conversions (Strings only!). This is designed for situations like the part number example described earlier: company A calls a part "abc" and company B calls the same part "xyz". Rather than a bunch of convoluted logic we just want these part numbers mapped in-flight.

We accomplish this by defining a BiMap (2-way Map) and a registry:
```scala 
val numbers = Map("abc"->"p123", "xyz"->"p456")
val withRegistry = (new BiMapRegistry()).register("testmap", BiMap.fromMap(numbers))   
```
In practice we don't care where these Maps come from: parsed JSON, database, etc. Get them where you like. For our example here they just are.

Then we run the script as usual but this time pass the registry:
```scala
val lens = dynalens[Shipment]
val inst = Shipment("aaa",List(Item("abc",9,5), Item("xyz",1,7)),1)
val scriptFwd = """items[].number = mapFwd("testmap")"""
for{    
  parsed <- Parser.parseScript(scriptFwd)
  result <- lens.run(parsed, inst, withRegistry) 
} yield result   
```
This example shows mapFwd function, which converts A->B. So in this case the resulting object will be:
```scala 
Shipment("aaa",List(Item("p123",9,5), Item("p456",1,7)),1)   
```   
The mapRev goes the other way: B->A

#### Optional Data
DynaLens supports safe and explicit handling of `Option`-wrapped values using intuitive syntax and built-in fallback mechanisms. This document explains how optional fields behave across expressions, assignments, and control structures.

#####  Reading `Option` Values

When you access an optional value, DynaLens requires you to explicitly provide a fallback using `.else(...)` unless you're using it in one of the following special cases:

##### Allowed Without `.else(...)`

- Within a `val` declaration
  ```scala
  val maybeThing = foo.bar.else("default") # .else() required
  ```

- When using `.isDefined()` or `.len()` on an optional collection
  ```scala
  val count = interest[].len()

  if interest[].isDefined() {
    ...
  }
  ```

##### Not Allowed Without `.else(...)`

- Using an `Option` directly in an assignment without a fallback
  ```scala
  maybeThing = y  # Error unless y.else(...) is used
  maybeThing = y.else(None)  # OK
  ```

---

#####  Semantics for RHS in Assignments

When assigning to a field (e.g., `foo.bar = something`), DynaLens unwraps the RHS `Option` first. If the RHS is optional and may be `None`, you must explicitly handle that with `.else(...)` — unless the RHS is a **pure path reference** (i.e., a raw field get), which is assumed to be safe and will be rewrapped automatically during assignment.

```scala
maybeThing = user.profile.name  # if user.profile.name is a direct field path--may be None
```

---

##### Option + Iterables

When working with optional lists (e.g., `Option[List[T]]`), DynaLens supports dereferencing with `[]`:

```scala
val itemCount = interest[].len()      # OK: optional list length
interest[].qty = 5                    # OK: map assignment
interest[].qty = interest[].len() * 5 # Currently unsupported — see notes below
```

##### Edge Case: RHS Expressions in Map Assignments

Due to current evaluation semantics, expressions like:

```scala
interest[].qty = interest[].len() * 5
```

are **not supported directly**. Reading the code it looks perfectly rational that it should work. But remember, DynaLens is not a general-purpose programming language, and the internals currently can't handle unwrapping an optional list in a mapping assignment like this (foo[].something)

#####  Recommended Workaround

```scala
val count = interest[].len()
interest[].qty = count * 5
```

This avoids repeated evaluation of `.len()` during each mapping iteration and keeps semantics consistent.

---

##### Option Detection Logic

Internally, the DSL engine treats any access ending in `[]` as iterable dereference and enforces `.else(...)` only for non-collection `Option` values. Certain functions (e.g., `.len()`, `.isDefined()`) are special-cased to allow more natural syntax.

Some rules of thumb:

- `foo.bar` (plain path to optional value): requires `.else(...)` unless part of a val or direct path assignment.
- `foo[]` (dereferenced iterable): does not require `.else(...)`.
- `foo[].len()` or `foo[].isDefined()` are always safe.
- `foo.bar.baz` on its own is never considered an iterable and may error if any segment is `None`.

---

#####  Summary
Assuming foo.bar and interest[] are both optional fields.

| Use Case                                 | Requires `.else()`?         |
|------------------------------------------|------------------------------|
| `val x = foo.bar.else("nada")            | yes |
| `foo.bar = something`                    | no |
| `val x = interest[].len()`               | no, this is fine with .len() |
| `interest[].qty = 5`                     | no, this is fine. The 5's will be Some()-wrapped |
| `interest[].qty = interest[].len() * 5`  | Use a `val` first as shown above  |

---

> ℹ️ Note: We may enhance support for more dynamic Option-aware expressions in the future. For now, use explicit `val` bindings and `.else(...)` clauses to ensure predictable behavior.
