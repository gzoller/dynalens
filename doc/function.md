

## dynalens Function Reference

#### Notes:
* Functions in dynalens always have () notation, even if there are no parameters specified.
* Functions associated with strings can apply to string literals (shown below), string-valued val's, or paths, for example foo.bar.contains("ok") if foo.bar is a string-valued field
* Boolean functions are intended to be used in comparitors, for example in an 'if' statement
* Most functions can be chained together: a().b()

|Function  | Description                                                                                                                                                     | Example                                                                                                                                    |  
|--|-----------------------------------------------------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------|  
|startsWith  | Boolean-string starts with value                                                                                                                                | "foobar".startsWith("foo")                                                                                                                 |  
|endsWith  | Boolean-string starts with value                                                                                                                                | "foobar".endsWith("bar")                                                                                                                   |  
|contains  | Boolean-string contains value                                                                                                                                   | "foobar".contains("oo")                                                                                                                    |  
|equalsIgnoreCase  | Boolean-string compare ignoring case                                                                                                                            | "FooBar".equalsIgnoreCase("foobar")                                                                                                        |  
|matchesRegex  | Boolean-string matches regex                                                                                                                                    | "FooBar".matchesRegex("F.*?B.*")                                                                                                           |  
|toUpperCase  | Convert string to upper case                                                                                                                                    | "foobar".toUpperCase()                                                                                                                     |  
|toLowerCase  | Covert string to lower case                                                                                                                                     | "FooBar.toLowerCase()                                                                                                                      |  
|template  | Populate a template string value<br>(string interpolation)                                                                                                      | "Hello {name}, your price is {ticket.price}".template()<br>*(values in {} are paths in default context, ie fields in<br>top-level object)* |  
|substr  | Extract substring from string                                                                                                                                   | "FooBar".substr(3) # Bar<br>"FooBar".substr(1,3) # oo                                                                                      |  
|replace  | Replace a matched string with another                                                                                                                           | "YooHoo".repalce("oo","aa") # YaaHaa                                                                                                       |  
|:: | String concatenation<br>non-string values converted to string                                                                                                  | "a" :: 4 :: "c" |  
|len  | String or Iterable length                                                                                                                                       | "Hello".len # 5<br>items[].len # num of items in list                                                                                      |  
|filter  | Filter an Interable, keeping items matching<br>predicate<br>*Note: Like an assignment, filter changes the<br>object in context. **It doesn't return a value!*** | items[].filter(this.price < 10.50)                                                                                                         |  
|mapFwd  | String value mapping                                                                                                                                            | (see Mapping below)                                                                                                                        |  
|mapRef  | String value mapping                                                                                                                                            | (see Mapping below)                                                                                                                        |
|uuid|Produce a random UUID|val id = uuid()|
|now|Produce current date|val rightNow = now()|
|dateFmt|Format a date per given Java date fmt |val old = when.dateFmt("MM-dd-yy")|
|toDate|Parse a string to date w/format|"07-09-1972".toDate("MM-dd-yyyy")|

#### Mapping
There are 2 special functions designed to do 2-way value conversions (Strings). This is designed for situations like the part number example described earlier: company A calls a part "abc" and company B calls the same part "xyz". Rather than a bunch of convoluted logic we just want these part numbers mapped in-flight.

We accomplish this by defining a BiMap (2-way Map) and a registry:
```scala  
val numbers = Map("abc"->"p123", "xyz"->"p456") val withRegistry = (new BiMapRegistry()).register("testmap", BiMap.fromMap(numbers))  
```  
In practice we don't care where these Maps come from: parsed JSON, database, etc. Get them where you like. For our example here they just are.

Then we run the script as usual but this time pass the registry:
```scala  
val lens = dynalens[Shipment]  
val inst = Shipment("aaa",List(Item("abc",9,5), Item("xyz",1,7)),1)  
val scriptFwd = """items[].number = mapFwd("testmap")"""  
for{  
 parsed <- Parser.parseScript(scriptFwd)  result <- lens.run(parsed, inst, withRegistry)  
} yield result  
```  
This example shows mapFwd function, which converts A->B. So in this case the resulting object will be:
```scala  
Shipment("aaa",List(Item("p123",9,5), Item("p456",1,7)),1)  
```  

The mapRev goes the other way: B->A