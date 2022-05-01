<style>
  html, body {
    font-family: sans-serif;
    max-width: 100ch;
    margin-left: auto;
    margin-right: auto;
  }

  @media print {
    pre, blockquote {page-break-inside: unset;}
  }
</style>

<!-- omit in toc -->
# A Whirlwind Tour of *Language o' Things*

Jonah Sussman
CWID 11966879
CS 503

<!-- omit in toc -->
## Table of Contents

- [1. Rationale](#1-rationale)
- [2. Crash Course: LoT](#2-crash-course-lot)
- [3. Expression Types](#3-expression-types)
  - [3.1 Trivial Types (Number, String, Boolean)](#31-trivial-types-number-string-boolean)
  - [3.2 Semi-trivial Types (Nil, Symbol)](#32-semi-trivial-types-nil-symbol)
  - [3.3 Non-trivial Types (List, Thing, Call, Primitive, Procedure)](#33-non-trivial-types-list-thing-call-primitive-procedure)
- [4. Primitives](#4-primitives)
  - [IO](#io)
  - [Types](#types)
  - [Arithmetic](#arithmetic)
  - [Comparison](#comparison)
  - [Logic](#logic)
  - [Ordering](#ordering)
  - [Control Flow](#control-flow)
  - [Procedures](#procedures)
  - [Variables](#variables)
  - [Thing Modification](#thing-modification)
- [X. Examples](#x-examples)
- [X. Grammar](#x-grammar)

## 1. Rationale

These days, it's incredibly easy to find programming languages that solve all sorts of problems. Highly-expressive languages like Haskell can tersely compute all sorts of problems while writing as few lines of code. Other languages concern themselves with squeezing the most amount of performance of the machine as possible. C++, for instance, gives you near full access to the machine's memory - safety be damned. Python is another language which aims to be extremely friendly to beginners, while still maintaining some semblance of performance.

Unfortunately, all of these aforementioned programming languages subscribe to certain dogmatic principles - often at to the detriment of the programmer. The creators of these languages take the beautiful problem space of all possible programs, and section off areas into clearly defined "DO"s and "DON'T"s according to their beliefs. Ask any Java programmer what a factory is and they'll want to tear their hair out.

It's often said that limitation breeds creativity, but why should we as software engineers limit ourselves so greatly. I don't ever recall a civil engineer being thrilled that they were forced to build a bridge using popsicle sticks, or a mechanical engineer being ecstatic that they could only use a slide rule and compass. Why should our field be any different?

Enter *Language o' Things*, or LoT. LoT aims to be a highly extensible, multi-paradigm language that doesn't box you in. It's small, in order to get out of your way, but highly-expressive. 

If you want to write in a functional style, you can! Functions are first-class. If you want to write in a data-oriented style, you can! Most data types are passed via reference. If you want to write in an object-oriented-style, you can! Things have methods which you can implicitly pass itself to, and Meta-Things act like superclasses.

## 2. Crash Course: LoT

The following is a simple crash course in LoT to get you familiar with the language. Following sections will expand on this file later.

```lua
#include "examples/2_crash_course.lot"
```

## 3. Expression Types

Everything in LoT is an expression. This means that everything that gets parsed by the language evaluates to something else in the language. Most of the time, it's a self-circular evaluation - meaning the expression evaluates to itself. Other times, like for Calls and Symbols, something special happens. 

There are 10 data types in LoT. They can be divided into 3 distinct categories:
- Trivial (Number, String, Boolean)
- Semi-trivial (Nil, Symbol)
- Non-trivial (List, Thing, Call, Primitive, Procedure)

### 3.1 Trivial Types (Number, String, Boolean)

The 3 trivial data types are Number, String, and Boolean. These data types should be fairly easy to understand.

**Number**: Numbers are implemented as IEEE double precision floating point numbers. 
**String**: Implemented as a standard C-style string.
**Boolean**: Implemented as a standard C boolean.

Each of these expression types are compared by *value*. So for example.

```lua
#include "examples/3_1_compare.lot"
```

You can also easily convert between these types.

```lua
#include "examples/3_1_convert.lot"
```

### 3.2 Semi-trivial Types (Nil, Symbol)

The 2 Semi-trivial data types are Nil and Symbol.

**Nil**: Internally, nil is represented by a singular object. Any internal object pointer that points to nil is considered nil.

**Symbol**: Internally, Symbols are represented nearly identically to strings. The only difference is that when a symbol is created, it gets "interned" into a global object map. Then their pointers are compared, rather than the object itself. Additionally, there is no way for a user to store a symbol in a variable. Symbols can only be the variables themselves.

### 3.3 Non-trivial Types (List, Thing, Call, Primitive, Procedure)

There are 5 non-trivial data types. These types are compared via reference, and usually cannot be compared using standard operators.

**List**: Used internally. While you can declare lists using `[ stuff, stuff, ... ]`, *it is highly recommended that you do not use them*. Lists are immutable, and you cannot directly access the elements. The are used primarily for passing arguments to functions internally.

**Thing**: Things can be thought of as simple key value containers. They associate expression to other expressions. Additionally, Things can have associated Meta-Things. If a key is not present, it 

```lua
#include "examples/3_3_things.lot"
```


At the heart of LoT is a simple LISP-style EVAL APPLY loop. Most things simply evaluate to themselves. Symbols evaluate to whatever is stored in the current environment. However, Calls are a bit special.

**Call**: A call consists of two things: an expression representing either the procedure or primitive to call, and a list of arguments. When evaluated, it will "apply" the arguments to the former expression.

**Procedure**: A procedure evaluates all of its arguments, creates a new environment to execute in, sets each of its parameters to the evaluated arguments, and evaluates the body statements it has one by one. It also listens for a `return expr` expression, and will return that expression instead if encountered.

**Primitive**: 

## 4. Primitives

The majority of the grammar is syntactic sugar to convert specific structures into these primitives. If you wanted to, you could write any LoT program using just functional notation - though I would heavily advise against this.

I will use the following syntax to describe the primitives:

- `~>` means `transforms into`, meaning the parser will change it from the left to the right. Not every primitive needs transforming.
- `->` means `returns`
- `...` means the primitive supports an arbitrary number of arguments

### IO

<!-- print, println, input, gettime -->
```lua
print(expr, ...) -> nil
  expr :: The expressions to print

println(expr, ...) -> nil
  expr :: The expressions to print, then a single newline

input(expr, ...) -> str
  expr :: The expressions to print
  str  :: The resulting string read from standard in

gettime() -> num
  num :: The current Unix time
```

### Types

<!-- Boolean, Number, String, __thing, __list, type -->
```lua
Boolean(expr) -> bool
  expr :: The expression to convert to a Boolean
  bool :: The truthiness of expr

Number(expr) -> res
  expr :: The expression to convert to a Number
  res  :: The number if successful, otherwise nil

String(expr, ...) -> str
  expr :: The expressions to convert to string.
  str  :: The concatenated resultant string

{ fieldlist? } ~> __thing(key, value, ...) -> thing
  key   :: The key of the field
  value :: The value to set. NOTE: key and values must come in pairs
  thing :: The resultant thing

[ expr, ... ] ~> __list(expr, ...) -> list
  expr :: The list of expressions to evaluate
  list :: The list of evaluated expressions

type(expr) -> str
  expr :: The expression to get the type of
  str  :: "Symbol", "String", "Number", etc...
```

### Arithmetic

<!-- __add, __sub, __mul, __div, __neg, mod -->
```lua
x + y ~> __add(x, y) -> num
  x   :: The first number
  y   :: The second number
  num :: The result of x + y

x - y ~> __sub(x, y) -> num
  x   :: The first number
  y   :: The second number
  num :: The result of x - y

x + y ~> __mul(x, y) -> num
  x   :: The first number
  y   :: The second number
  num :: The result of x * y

x + y ~> __div(x, y) -> num
  x   :: The first number
  y   :: The second number
  num :: The result of x / y

mod(x, y) -> num
  x   :: The first number
  y   :: The second number
  num :: The result of x modulo y

-x ~> __neg(x) -> num
  x   :: The number to negate
  num :: The negation of x
```

### Comparison

<!-- __eq, __neq -->

### Logic

<!-- __not, __and, __or -->

### Ordering

<!-- __gt, __geq, __lt, __leq -->

### Control Flow

<!-- __do, __if, __while -->

### Procedures

<!-- __fun, __return -->

### Variables

<!-- __let, __assign -->

### Thing Modification

<!-- __get, __set, __selfcall, getmeta, setmeta -->

## X. Examples

## X. Grammar

The following is the grammar of LoT, in roughly a EBNF-style form

```bnf
#include "presentation/grammar.bnf"
```