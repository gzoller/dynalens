

## dynalens -- script-driven, dynamic lens mutation for compiled Scala case class fields

[![license](https://img.shields.io/github/license/mashape/apistatus.svg?maxAge=86400)](https://opensource.org/licenses/MIT)  
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/co.blocke/dynalens_3/badge.svg)](https://search.maven.org/artifact/co.blocke/dynalens_3/1.0/jar)

The intent of this library is to allow for a runtime-interpreted script to modify a compiled Scala case class. Imagine you have a business object Shipment, which has Items. Let's say these objects flow in an event stream and you want to be able to mutate certain values in-flight. The challenge is these mutations vary at runtime, for example per client, and the person in your company with the business logic knowledge isn't a software engineer. Their coding skill is at an Excel macro level.

What if there were a way to use a simple scripting language to apply changes to the in-flight objects? That's what dynalens does.

### Script Example

```scala  
// Scala Business objects  
case class Item(number: String, qty: Int, price: Double)
case class Shipment(id: String, items:List[Item], shipMethod: Int)  
```  

```  
# My script  
shipMethod = 3 # set a top-level field  
items[].qty = this * 2 # change all qty fields of all items  
items[3].qty = this / 2 # change qty fields only of items[3]  
items[3].qty = shipMethod # value set from top-level  
```   
The ability to change values can span multiple collections (lists). Let's add another couple layers of business object:

```scala  
case class Pack(label: String, caseSize: Int, shipments: List[Shipment])
case class Order(id: String, pack: Pack)  
```  
If Order is the top-most object we can still change all the qty values of all the items:
```  
pack.shipments[].items[].qty = 1 # set all qty to 1 for all items  
```  
In this example we iterated over 2 levels of nesting (shipments and items) with one simple line of assignment code.

### Installation
In your build.sbt file add the current version of the library to libraryDependences:

```scala  
libraryDependencies += "co.blocke" %% "dynalens" % CURRENT_VERSION  
```  
(CURRENT_VERSION value can be taken from the 'maven central' badge in this github repo.)

### Usage
dynalens is a Scala 3 macro written to work with ZIO. If you haven't ever worked with ZIO, don't be intimidated. At a high level it's a lot simpler than it appears. The main nugget here is that it provides a pervasive error channel so you don't have to "sew" exception handling through your code. Here's how you set it up:

##### 1. Write your Scala Case Classes
```scala
case class Person(name: String, age: Int, id: String)
```
##### 2. Create an instance of your class
```scala
val inst = Person("Mike", 45, "abc")
```
##### 3. Write a script
```
val script = "age = age * 2" // This can come from anywhere in practice--eval'ed at runtime
```
##### 4. Set up the parse/run for comprehension
```scala
val lens = dynalens[Person]
for{  
  compiledScript <- Parser.parseScript(script)  
  (result,_) <- lens.run(compiledScript, inst)  
} yield result // ZIO[Any,DynaLensError,Person]
```
### Non-ZIO Usage
We really love ZIO around here, but if you just can't get there, there's a non-ZIO flavor that uses Either. Steps 1-3 as above.
Step 4 becomes:
```scala
val lens = dynalens[Person]
val result: Either[DynaLensError, (Shipment, Map[String, (Any, DynaLens[?])])] =  
  for {  
    compiled <- Parser.parseScriptNoZIO(script)  
    (output,_) <- lens.runNoZIO(compiled, inst)  
  } yield output
```

### dynalens Script Language Reference
* [Documentation](doc/language.md)
* [Function Reference](doc/function.md)

### Release History
* 1.0.0 - Initial Release