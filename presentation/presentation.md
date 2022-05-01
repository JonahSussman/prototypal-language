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
  - [4.1 IO](#41-io)
  - [4.2 Types](#42-types)
  - [4.3 Arithmetic](#43-arithmetic)
  - [4.4 Comparison](#44-comparison)
  - [4.5 Logic](#45-logic)
  - [4.6 Ordering](#46-ordering)
  - [4.7 Control Flow](#47-control-flow)
  - [4.8 Procedures](#48-procedures)
  - [4.9 Variables](#49-variables)
  - [4.10 Thing Modification](#410-thing-modification)
- [5. Examples](#5-examples)
  - [5.1 Bottles of Milk](#51-bottles-of-milk)
  - [5.2 Higher-Lower Guessing Game](#52-higher-lower-guessing-game)
  - [5.3 Tail-call-optimization](#53-tail-call-optimization)
  - [5.4 Vector](#54-vector)
  - [5.5 Tree Traversal](#55-tree-traversal)
- [6. Grammar](#6-grammar)

## 1. Rationale

These days, it's incredibly easy to find programming languages that solve all sorts of problems. Highly-expressive languages like Haskell can tersely compute all sorts of problems while writing as few lines of code. Other languages concern themselves with squeezing the most amount of performance of the machine as possible. C++, for instance, gives you near full access to the machine's memory - safety be damned. Python is another language which aims to be extremely friendly to beginners, while still maintaining some semblance of performance.

Unfortunately, all of these aforementioned programming languages subscribe to certain dogmatic principles - often at to the detriment of the programmer. The creators of these languages take the beautiful problem space of all possible programs, and section off areas into clearly defined "DO"s and "DON'T"s according to their beliefs. Ask any Java programmer what a factory is and they'll want to tear their hair out.

It's often said that limitation breeds creativity, but why should we as software engineers limit ourselves so greatly. I don't ever recall a civil engineer being thrilled that they were forced to build a bridge using popsicle sticks, or a mechanical engineer being ecstatic that they could only use a slide rule and compass. Why should our field be any different?

Enter *Language o' Things*, or LoT. LoT aims to be a highly extensible, multi-paradigm language that doesn't box you in. It's small, in order to get out of your way, but highly-expressive. 

If you want to write in a functional style, you can! Functions are first-class. If you want to write in a data-oriented style, you can! Most data types are passed via reference. If you want to write in an object-oriented-style, you can! Things have methods which you can implicitly pass itself to, and Meta-Things act like superclasses.

The programmer should feel free to choose the tool that fits the job. LoT provides an environment to create and use these tools freely.

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

### 4.1 IO

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

### 4.2 Types

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

### 4.3 Arithmetic

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

### 4.4 Comparison

<!-- __eq, __neq -->
```lua
x == y ~> __eq(x, y) -> bool
  x    :: The first expression
  y    :: The second expression
  bool :: Returns true if the objects are equal (by value for trivial), otherwise false.

x != y ~> __neq(x, y) -> bool
  x    :: The first expression
  y    :: The second expression
  bool :: Returns false if the objects are equal (by value for trivial), otherwise true.
```

### 4.5 Logic

<!-- __not, __and, __or -->
```lua
!x ~> __not(x) -> bool
  x    :: The expression to evaluate
  bool :: The opposite truthy value of x

x and y ~> __and(x, y) -> bool
  x    :: The first expression
  y    :: The second expression
  bool :: true if x and y are truthy, otherwise false

x or y ~> __or(x, y) -> bool
  x    :: The first expression
  y    :: The second expression
  bool :: true if x or y are truthy, otherwise false
```

### 4.6 Ordering

<!-- __gt, __geq, __lt, __leq -->
```lua
x > y ~> __gt(x, y) -> bool
  x    :: The first expression
  y    :: The second expression
  bool :: true if x > y, otherwise false

x >= y ~> __geq(x, y) -> bool
  x    :: The first expression
  y    :: The second expression
  bool :: true if x >= y, otherwise false

x < y ~> __lt(x, y) -> bool
  x    :: The first expression
  y    :: The second expression
  bool :: true if x < y, otherwise false

x <= y ~> __leq(x, y) -> bool
  x    :: The first expression
  y    :: The second expression
  bool :: true if x <= y, otherwise false
```

### 4.7 Control Flow

<!-- __do, __if, __while -->
```lua
do expr ... end ~> __do(expr, ...) -> res
  expr :: The expressions to evaluate, in order
  res  :: The last expression, evaluated

if a then e ( elif b then e )* ( else e )? end ~> __if(a, e, ...) -> res
  a, b, c :: The conditions
  e       :: The expressions to evaluate
  res     :: The last expression evaluated

while cond do expr ... end ~> while(cond, expr, ...) -> res
  cond :: The condition of the loop
  expr :: The expressions to evaluate
  res  :: The last expression, evaluated. If none, then nil
```

### 4.8 Procedures

<!-- __fun, __return -->
```lua
fun [s, ...] expr ~> __fun(list, expr) -> res
  s    :: The symbols for the parameters
  list :: The list of parameters
  expr :: The expression representing the procedure
  res  :: The result of the procedure 

return expr ~> __return(expr) -> expr
  expr :: The expression to return
```

### 4.9 Variables

<!-- __let, __assign -->  
```lua
let sym = expr ~> __let(sym, expr) -> expr
  sym  :: The symbol to set
  expr :: The expr to set the symbol to

sym = expr ~> __assign(sym, expr) -> expr
  sym  :: The symbol to set
  expr :: The expr to set the symbol to
```

### 4.10 Thing Modification

<!-- __get, __set, __selfcall, getmeta, setmeta -->
```lua
thing.sym   ~> __get(thing, sym) -> res
thing[expr] ~> __get(thing, expr) -> res
  thing :: The thing to get values from
  sym   :: The symbol to use as a key
  expr  :: The expression to use as a key
  res   :: The value if found when accessing, otherwise nil

thing.sym   = val ~> __set(thing, sym, val) -> val
thing[expr] = val ~> __set(thing, expr, val) -> val
  thing :: The thing to get values from
  sym   :: The symbol to use as a key
  expr  :: The expression to use as a key
  val   :: The value to set

thing:proc(args, ...) ~> __selfcall(thing, proc, args) -> res
  thing :: The thing to access
  proc  :: The procedure to call
  args  :: The argument to the procedure
  res   :: The result of the procedure
  
getmeta(thing) -> meta
  thing :: The thing to access
  meta  :: The Meta-Thing of thing if exists, nil otherwise

setmeta(thing, meta) -> thing
  thing :: The thing to set the Meta-Thing for
  meta  :: The metathing itself
```

## 5. Examples

### 5.1 Bottles of Milk

Here's a classic program, bottles of milk! 

```lua
#include "examples/5_1_bottles_of_milk.lot"
```

### 5.2 Higher-Lower Guessing Game

Here's an example of the classic "higher or lower" guessing game

```lua
#include "examples/5_2_hi_lo.lot"
```

### 5.3 Tail-call-optimization

Here's an example of tail-call optimization

```lua
#include "examples/5_3_tail_call.lot"
```

### 5.4 Vector

Here's an implementation of a c++ style vector

```lua
#include "examples/5_4_vector.lot"
```

### 5.5 Tree Traversal

Here's an example of different types of tree traversals

```lua
#include "examples/5_5_dfs.lot"
```

## 6. Grammar

The following is the grammar of LoT, in an EBNF-style form

```bnf
#include "presentation/grammar.bnf"
```