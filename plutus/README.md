# Plutus

Look, I'm not an expert in algorithms for parsing Text. I just read a little bit and tried some other libraries (Scala Parser Combinators and Fastparse) and none gave me what I was looking for. The main problem I faced was this:

Imagine I have a language with the keyword `switch` and a rule for Identifiers with the following regex `[a-zA-Z]+`. With the libraries I mentioned before I will run into the problem of correctly processing the identifier `switcheroo`, why? Because it either recognize the keyword `switch` followed by the identifier `eroo` or it doesn't recognize `switch` as a keyword but an identifier.

Another problem is: When to conclude the current rule and proceed to the next? Easy, when there's a whitespace, right? (well, at least in most languages). But what if you have this `max(1,2)`? There's no spaces in this expression and still you'd want it to be a valid expression if you were creating a Java-ish grammar.

And last but not least: When creating languages you usually want the capabilities of a parsing library to do two different things: Parsing (duh!) and lexing (or tokenizing). The problem with Scala Parser Combinators and fastparse is that they only work with Strings... what if I want to parse combinations of other types of data like `Int`s or some custom data type that represents the tokens of my language?

Enter plutus. Plutus defines what I call a `Packer[Src,El,R]`. which is basically an alias for this function:

`Cursor[Src,El]=>PackerResult[R]`

A very very simple concept but yet, so extremely powerful.

So let's explain every part in this concept:

`Src` is the type of the source code you're trying to process. Typically `String`, `Seq` of some user defined tokens or `File`

`El` is the type of each element of the source code. Typically `Char`, `Byte` or some user defined Token type.

`Cursor` is an immutable tuple of a reference to the source code and a position in that source code.

`PackerResult[R]`: The result of a running a `Packer`. It can either be `Done` or `Failed`. If `Done`, it will have a value of type `R` and a new position in the source code. If `Failed`it will have a collection of Failures each of them will have a message indicating the error message and a position in which the Packer failed.

This very simple concept will give for a very powerful way of processing text.

## An example

Let's create a simple `Json` parser with this

A Json is compose by 6 types of values: numbers, booleans, nulls, strings, objects and arrays.

As we will be processing String, let's import `StringSyntaxSugar`

```scala
import StringSyntaxSugar._
```

Which includes a set of methods for creation of commonly used `Packer`s. 

Let's start with the Packer for nulls:

```scala
lazy val nullPacker = P("null")`
```

Pretty simple, huh? `P` will help you create Packers out of predicates (`El=>Boolean`), `Src`s, or a collection of `El`s. In this case as `Src` is set to be`String`, I can do `P("null")`. This `Packer`, when used will move the cursor one at a time and compare each element in the `"null"` string with each element in the source code. `P('n','u','l','l')` would be a perfect equivalent of it.

Let's do booleans now

```scala
lazy val booleanPacker = P("false") | P("true")
```

This one is more complex, but still simple enough. the `Or` operator actually means `greediestOf` so that is equivalent to `Pakcer.greediestOf(P("false"),P("true"))`. As its name indicates, it will chose the `Packer` which moves the cursor closer to the end of the source code (the greediest). If both are just as greedy, it will choose the first one. If all fail, it will fail with all the errors reported by each packer.

Now numbers

```scala
lazy val digits = P(_.isDigit).rep(min = 1)

lazy val exponent = P("e|E".r) ~ P("""[+\-]""".r).? ~ digits

lazy val fractional = P(".") ~ digits

lazy val integral = (P("0") | P("""[1-9]""".r)) ~ digits.?

lazy val numberPacker = P("""[+\-]""".r).? ~ integral ~ fractional.? ~ exponent.?
```

Way more difficult, but if you check it, it's pretty simple too. `StringSyntaxugar` also allows you to create `Packer`s out of Regular expressions so that explains all the `Packer`s with a `.r` after a string.

The `?` operator means one or none. So basically `anyPacker.?` is equivalent to `anyPacker | noop` where `noop` is a `Packer` that always succeed and doesn't move the cursor.

`~` is an extension method that allows you to concatenate `Packer`s. Under the hood uses the `flatMap` method (Yes, `Packer`s have monadic properties, yei!). `flatMap`basically concatenates two Packers, so the Second `Packer` takes the cursor where the first `Packer` left (in case the first one succeeded).
So `P(".") ~ digits` is roughly equivalent to `P(".").flatMap(_ => digits)`.

`rep` allows you for more granular sequential repetition of a `Packer`, you can establish a maximum, no maximum, a minimum number of repetitions and even a separator `Packer` which we'll use in the following cases.

Now Strings, we're almost done.

```scala
lazy val unicodeEscape = P("u") ~ hexDigit.rep(min=4,max=Some(4))

lazy val escape = P("\\") ~ (P("[\"/\\\\bfnrt]".r) | unicodeEscape)

lazy val strChars = P("""[^\\\"]*""".r)

lazy val stringPacker = P("\"") ~ (strChars | escape).rep ~ P("\"")
```

There's no new concepts here, so let's move to arrays and objects but first define or parent rule. The one rule to rule them all and bind them into... whatever

```scala
lazy val objectPacker = ???
lazy val arrayPacker = ???
lazy val jsonPacker = nullPacker | booleanPacker | numberPacker | stringPacker | arrayPacker | objectPacker
```

We need to define the jsonPacker first because objectPacker and arrayPacker will use it (they are what make this a recursive grammar) and that's the very reason we are using `lazy val`ues

Array packer:

```scala
lazy val arrayPacker = P("[") ~ jsonPacker.rep(sep=P(",")) ~ P("]")
```

Object packer:

```scala
lazy val objectPacker = P("{") ~ (stringPacker ~ P(":") ~ jsonPacker).rep(sep=P(",")) ~ P("}")
```

Now we are done... but what about the whitespaces? They are only needed in the object and array packers and it can be fixed like this:

```scala
lazy val space = P(_.isWhitespace).rep
lazy val arrayPacker = P("[") ~ (space ~ jsonPacker).rep(sep= space ~ P(",")) ~ space ~ P("]")
lazy val objectPacker = P("{") ~ (space ~ stringPacker ~ space ~ P(":") ~ space jsonPacker).rep(sep=space ~ P(",")) ~ space ~ P("}")
```

Now we are done. Yet it only validates if a Json is correct or not.

If we want to create a Json structure that can be easily traversed in `scala`?

Let's modify it so that it parses while creating given structure:

```scala
sealed trait JSON {}
object JSON {
  case object JSNull                            extends JSON
  case class JSBool(value: Boolean)             extends JSON
  case class JSNumber(value: BigDecimal)        extends JSON
  case class JSText(value: String)              extends JSON
  case class JSArray(value: Seq[JSON])          extends JSON
  case class JSObject(value: Map[String, JSON]) extends JSON
}
```

First let's deal with the easy cases

```scala
lazy val nullPacker    = P("null").as(JSNull)
lazy val booleanPacker = (P("false") | P("true")).!.map(window => JSBool(window.value.toBoolean))
lazy val digits        = P(_.isDigit).rep(min = 1)

lazy val exponent = P("e|E".r) ~ P("""[+\-]""".r).? ~ digits

lazy val fractional = P(".") ~ digits

lazy val integral = (P("0") | P("""[1-9]""".r)) ~ digits.?

lazy val numberPacker =
  (P("""[+\-]""".r).? ~ integral ~ fractional.? ~ exponent.?).!.map { window =>
    JSNumber(BigDecimal(window.value))
  }
```

the `as` extension method ignores the current result and uses the provided one. In this case `JSNull`

the `!` operator ignores the value returned by the packer and replaces it with a `Window`. A `Window` is a reference to the source code and two positions, the position at which the `Packer` started consuming the cursor and the position at which ended consuming the cursor. You can extract the slice of the source code using `.value` on the window. `map` is an even more powerful method that allows you to apply a method on the value captured by a `Packer` just like the `map` method in `List`s and `Future`s.

Now let's finish with the complex cases

```scala
lazy val hexDigit = P("[0-9a-fA-F]".r)

lazy val unicodeEscape = P("u") ~ hexDigit.rep(min = 4, max = Some(4))

lazy val escape = P("\\") ~ (P("[\"/\\\\bfnrt]".r) | unicodeEscape)

lazy val strChars = P("""[^\\\"]*""".r)

lazy val stringPacker = (P("\"") ~ (strChars | escape).rep ~ P("\"")).!.map { window =>
  JSText(unescapeJsonString(window.value))
}
lazy val space: Packer[String, Char, Unit] = P(_.isWhitespace).rep.ignore
lazy val arrayPacker = (P("[") ~ (space ~ jsonPacker).rep(sep = space ~ P(",")) ~ space ~ P("]"))
  .map(JSArray.apply)
lazy val objectPacker = (P("{") ~ (space ~ stringPacker.map(_.value) ~ space ~ P(":") ~ space ~ jsonPacker)
  .rep(sep = space ~ P(",")) ~ space ~ P("}")).map{tuples=>
  JSObject(tuples.toMap)
}
lazy val jsonPacker
  : Packer[String, Char, JSON] = nullPacker | booleanPacker | numberPacker | stringPacker | arrayPacker | objectPacker
```

The `ignore` replaces the value of a packer for `Unit`. `rep` doesn't only repeat a packer; it also collects the results in a vector. So if `packerA: Packer[String,Char,String]` then `packerA.rep : Packer[String,Char,Vector[String]]`

And it's done. Easy peasy

Now you can do this:

```scala
(space ~ jsonPacker ~ space ~ End) //End succeeds only if the cursor is at the end of the
// source code, otherwise fails. This ensures the Packer won't leave some missing part
  .take("""
		{
			"name": "Alejandro",
			"age": 30
		}
""")
```

and it will give you a result wrapping the value you'd expect: `JSObject(Map(name -> JSText(Alejandro), age -> JSNumber(30)))`

