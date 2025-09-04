# Path Reference

This document covers how **paths** work in the DynaLens DSL. Paths point to fields in the input object and are the backbone of data access and updates.

---

## Basic Path Syntax

A **path** is a dot-separated reference to a field or subfield:

```
foo
foo.bar
foo.bar.baz
```

Each segment references a nested field in the Scala case class given to the lens.

---

## Indexed Paths (`[]`)

Paths can include `[]` to refer to **each element** in a collection:

```
orders[].qty
items[].details[].price
```

This enables mapped updates or expression chaining over each element.

Note: Use of '[]' syntax is optional, for your clarity. DynaLens knows which fields are lists, so this script is equivalent:

```
orders.qty
items.details.price
```

---

## Field Resolution Rules

- Paths resolve **relative to the current input context**.
- During collection mapping, the context switches to the current item.
- You can access parent fields from within mapped or nested contexts using lexical scoping:

```
items.flag = qty > limit
```

Here, `qty` is from `items.limit` is resolved from outer scope. To be more precise, you can also say:

```
items.parts.flag = this.qty > limit
```
Here we have double nesting (items and parts). If `limit` were a field in the Item class, you can reference the *current* Item like this:

```
items.parts.flag = this.qty > items.limit
```
This works because DynaLens creates pseudo symbols (`items` and `parts` in this case) for each level of a iterable loop, and populates it with the *current* item in that iteration. 

---

## 🔎 Optional and Missing Paths

If a field might be missing (`Option` or null), use `.isDefined()` or `.else()`:

```
email.isDefined()
contact.info.phone.else("Unknown")
```

### Optional Collection Fields

Optional lists are also supported:

```
things[].isDefined()     // things: Option[List[_]]
things[].qty = 0         // update each qty element if list is present
```

---

## Path Errors

A path may be invalid if:

- The field doesn't exist
- An intermediate field is null (unless `.else()` is used)
- You attempt to access a collection element on a non-collection field

These errors are surfaced as `DynaLensError` with detailed location.

---

## Related Docs

- [Documentation Main](dyna_lens_documentation.md)
- [Grammar](grammar.md)
- [Functions](functions.md)
- [Paths](paths.md)
- [Statements](statements.md)
- [Errors](errors.md)
- [Types](types.md)

