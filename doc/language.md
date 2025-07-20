
## dynalens Language Reference

#### Basic Assignment
```
<path> = <value expr>
```

#### Comments
```
# Stand alone comment
foo = 5 # end-of-line comment
```

#### val
```
val x = item.price * 1.25
```
A val is like a Scala val. Once set it cannot be changed. The symbol 'x' is added to the language context so it may be used as a path or as part of a computed value expr, like so:
```
item[0].price = x
```
val's allow you to compose your script logically without needing large statements that are hard to read.

#### Iterables (eg List)
Iterables like List, Seq, etc, are shown in a path with [] symbols, like this:
```
items[] # all items
items[2] # specific item
```
The dynalens language is not a general-purpose scripting language so you cannot specify an Iterable as th e LHS of an assignment:
```
items[] = ... # Illegal!
```
There are a few exceptions to this rule, but we'll cover that later. The short explanation is; to keep the language simple and managable, you aren't allowed to assign a list of something to a list field in your case class. You can only modify its values.

#### Conditional Logic
A simple if-then-else feature is provided:
```
val y = if x < 5 then "low" else "high"
```
In this case the 'if' logic is assigning a value, so there must be an 'else' clause. When an 'if' is used as a statement the 'else' is optional:
```
val x = 5
if x < 10 then
  shipMethod = 0
# No else required--top-level object mutated as a statement. If x >= 10 then nothing changes
```
All the usual boolean operators are provided:
* && (and)
* || (or)
* ! (not)
```
if x < 10 && !(y != "low") then
 ...
```
As expected, parentheses separate precedence.
The normal range of boolean comparators are likewise provided:
* ==
* &gt;
* &lt;
* !=
* &gt;=
* &lt;=

There are others that apply to String values (eg startsWith()) we'll cover in the function reference.

#### Blocks
Braces denote blocks of statements
```
val x = 5
if x < 10 then {
  shipMethod = 0
  items[].qty = 0
}
```
Here we have 2 mutations on top-level object (Order) if x < 10: set shipMethod to 0 and change all items' qty to 0.

#### Context
Contex is one of those terms that can mean many things. The default context is the top-level object. Consider our example on the main page with 4 levels of business object: Order->Pack->Shipment->Item,
```
id = 3 # default context = Order, so script looks for "id" at top level
pack.label = "foo" # descend into Pack, but still from default context of Order
```
Things get more interesting when changing things inside of nested lists:
```
pack.shipments[].items[].qty = this * 2
```
Here we introduce "this", which refers to the lowest-level *current* item in a loop. In this example we're conducting 2 loop iterations: one for shipments and an inner loop for items. "this" refers to the current item's qty field.

Ok, but what if I want to refer to another field on the current loop item? We can do this like so:
```
pack.shipments[].items[].qty = this * items.price
```
For each loop level, dynalens inserts a symbol named after that field into the context, but without the "[]". Used this way (and inside a looped assignment) "items" refers to the current item. So here we're taking "this" (current item's qty) and multiplying it by current item's price. Similarly we can access something like shipments.shipMethod, which is the current shipment's shipMethod.