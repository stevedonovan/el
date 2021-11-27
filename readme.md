## A Better Scientific Calculator, Done Unscientifically 

This all started because I became unsatisfied with console calculators, and wanted to move beyond keeping interactive Python open. My love for Lua made me type this a few too many times:

```sh
$ lua -e 'print(math.sin(2*math.pi))'
-2.4492935982947e-16
```
That `print` is irritating, as well as the carefully namespaced globals. But I find the quotes particularly annoying. 

So the first step, make printing implicit, put everything into global, and replace '()' by '[]' to avoid freaking out the shell. 

```sh
$ el sin[2*pi]
-2.4492935982947e-16
```
That's definitely easier on the fingers! Making the fingers do as little possible work for a given result starts making sense when your hands ache after a day of furious typing. (Unlike modern people, I don't think the solution is a new keyboard)

There's another simplification: if the expression is a simple function call, we collect the argument expressions separated by spaces:

```sh
$ el sin 2*pi
-2.4492935982947e-16
$ el max 10 5 23
23
$ el time
1636196164
$ el date
Sat Nov  6 12:54:19 2021
```
It's refreshing to have finger-friendly access to the Lua standard library without too much bracketing and commafication.

The argument expressions can be simularly written, if so desired. So we can party like it's 1963 with distinctly Lisp-ish syntax:

```sh
$ el max 20 {min 30 10}
20
```
There is a further rule; if the arguments are of form `VAR=EXPR` then these are considered key-value pairs and the function receives a _table_:

```sh
$ el a=1 b=2
{b=2,a=1}
```
`el` defines a global `set` which writes key-value pairs to a little `~/.el` file, which is then reloaded on each invocation.

```
$ el set tau=2*pi
$ el sin tau
1.3965925355373e-14
```
We would like our own functions to benefit from such convenient access, and Lua files can be registered with `el` using `set`.

```lua
-- normal.lua
local spi = math.sqrt(2*math.pi)
function normal(x,sig,mean)
  if sig == nil then 
    mean = x.mean
    sig = x.sig
    x = x.val
  end
  return 1.0/(sig*spi) * math.exp(-0.5*((x - mean)/sig)^2)
end
```
There is now a `dofile("/home/steve/dev/simple.lua")` in `~/.el` and so it is available globally. (Scary shit, but worse things live in '~')

```sh
$ el set add_file=^normal.lua
$ el normal 11 0.5 10
0.10798193302638
$ el normal mean=10 sig=0.5 val=11
0.10798193302638
```
The implicit table feature makes named arguments straightforward. The testing workflow is editing the file and exercising the functions using `el`.

Can also bring in Lua modules:

```sh
$ el set add_mod=^lfs
$ el lfs.dir ^. do it
data.txt
basic.lua
collect.lua
test.sh
...
```
You can save aliases to functions within modules, but there is a little catch:

```sh
$ el set dir=^lfs.dir
$ el dir ^. do it
...
```
The expression has to be quoted, because after it is evaluated, we have _no idea_ where the function came from (and in fact, not even its name without using the debug interface). So it must be a 'dotted expression' which we look up and confirm that it does resolve and handled specially.

Now, I have not forgotten that the _original itch_, before the explosion of curiosity, was to make a convenient technical calculator. We work with bits, so support for binary is useful & educational. Lua does not have binary literals, but we can make something similar happen at lookup time - check if it starts with 'b' and follows with binary digits. Bits go from least significant up, as is the usual tradition:

```sh
$ el b011
6
```
There are three output conversion functions: `json`, `hex` and `bin`. The special sauce here is that the first word of a top-level expression can be a conversion function. In this way, the cool Lua 5.2+ `bit32` bitwise functions can be explored:

```sh
$ el bin band b011 b010 
01
```
This convention saves us from having to say `bin {band b011 b010}` when applying an output conversion.

Here we print out the bits of a byte:

```sh
$ el 0 7 do print it {extract b01011010 it}
0	0
1	1
2	0
3	1
4	1
5	0
6	1
7	0
```

At this point, `el` is sufficient for the original purpose, of _doing calculations_ on the command line, with the extra coolness of saved variables and custom functions. But it's always fun to take an idea too far...

## Quoting Strings

The standard UNIX shell is awkward with quoting strings. Merely single (or double) quoting is insufficient:

```sh
$ el date '%c'
error	[string "expr"]:1: unexpected symbol near '%'	date(%c)
```
Instead, you have to say `'"%c"'` which is awkward. So strings are prefixed with '^' which has no special meaning to the shell; there are only a few sigils available which wouldn't also interfere with Lua operators.

```sh
$ el date ^%c
Sat Nov  6 14:24:47 2021
$ el date ^'%F %T'
2021-11-06 14:25:28
```
Could we infer from context? It is impossible to do this perfectly, and possible to create silent ambiguities, and 80% correct forces the user to second-guess a parser/evaluator.

Because of shell quoting rules, there are limitations. You can say `f[^'hello dolly',^fine]` but actually those single quotes are just making the statement more readable - the quotes disappear and so I do barbaric stuff like look for ',',']', etc. When in doubt, say '"hello dolly"'. (This is definitely one of the least attractive parts of `el` notation) 

There is something equivalent to Perl's `qw{...}`. If the function *ends* with '^', then all the arguments of that function are simply interpreted as quoted strings.

```sh
$ el list^ hello dolly fine
{"hello","dolly","fine"}
``` 
This makes collecting the results of a shell expansion possible:

```sh
$ el files={list^ *.lua }
{files={"1311-el.lua","basic.lua","bin.lua","collect.lua","el.lua","normal.lua"}}
```
Note the important space after `*.lua` for the shell expansion to work!

In the special case of subexprs, `{^ one two}` is the shortcut for `{list^ one two}`.

Also (borrowed from the convention set by `curl` and others) a file may be directly read in as a string using `@filename`. The special filename `stdin` means 'read standard input'.

```sh
$ el split @text.txt ^'\n'
{"Normally text isn't as interesting","as a line from a poem,","or a sentence scrawled in lipstick","on the bathroom mirror"}
```

## Hit and Miss: it and this

We can calculate now, but not repeat calculations. The keyword `do` separates the command-line into two expressions, the first is an iterator, the second an expression that will consume the iterator. The first value passed by the iterator is `it` (as in Kotlin) and the second value is `this`. The value of the expression will be printed out if `it` is not `nil`. (Generally you should treat `it` as reserved word since weird shit will happen if you use it in other contexts.)

```sh
$ el seq 0 4 do it
0
1
2
3
4
```
The global `seq` works rather like the usual Linux command of that name, but it can do floating point!

```sh
$ el seq 0 2*pi 0.2 do print it sin[it]
0	0
0.2	0.19866933079506
0.4	0.38941834230865
0.6	0.56464247339504
0.8	0.71735609089952
1	0.8414709848079
1.2	0.93203908596723
....
```
The formatting is crap, though. This looks better:

```sh
$ el seq 0 2*pi 0.2 do format ^'%5.1f %6.2f' it sin[it]
  0.0   0.00
  0.2   0.20
  0.4   0.39
  0.6   0.56
  0.8   0.72
  1.0   0.84
...
```
There are some implicit functions, depending on the content. `seq` is the default function for the iterator, if the first argument is a number. And `format` is the default for the expression, if the first argument is a string.

```sh
$ el 0 2*pi 0.2 do ^'%5.1f %6.2f' it sin[it]
  0.0   0.00
  0.2   0.20
  0.4   0.39
  0.6   0.56
  0.8   0.72
  1.0   0.84
  ...
```

Another place `it` appears when continuing a set of expressions using `:`. Personally I prefer the flow of data from left to right, rather than the right to left buildup of function application, so each expression is assigned to `it` in turn. The last expression's
value is printed out as usual (unless it is `nil`). This is also good for interactive refinement by adding modifiers, like shell pipes.

```sh
$ el ^'hello dolly' : sub it 1 4 : upper it
HELL
```

`this` appears in the common case of an iterator returning two values:

```sh
$ el pairs n=20 msg=^hello do print it this
n	20
msg	hello
```
Another context-specific implicit happens with _table constructors_:

```sh
$ el json {10 20 30}
[10,20,30]
$ el json {a=1 b=1.5}
{"a":1,"b":1.5}
```
Here the implicit function is `list`.

These implicit functions kick in when the expression does not start with a _known global function_. Can always use `eval` to force a subexpression to evaluate as an actual expression, not a list:

```sh
$ el 10 do printf {eval it%2==0 and ^'%ws ' or ^'%rs '} it
``` 

## Lambdas and User-Defined Functions

I've always wanted a shortcut form for `function(a,b) return a+b end` but most Lua people resist this idea, since (a) explicit is good (b) using new sigils would be a break from (mostly) keyword-driven syntax and (c) writing code is not as important as reading code. And these are all good points, if you are *programming*. But here, we are writing shell one-liners.

The `el` notation is a little different from the usual proposals, whether LHF's `\a,b(a+b)` lambdas or Rust-style `|a,b| a+b` closures:

```sh
$ el sort {5 2 3 6 1} {a,b: a '>' b} 
{6,5,3,2,1}
```
And the main reason is that the characters they use would fight with the shell, and we would *have to quote* again. (In this case, we did have to quote 'greater than' for this reason).

You can save these functions and reuse them!

```sh
$ el set desc={a,b: a '>' b}
$ el sort {5 2 3 6 1} desc
{6,5,3,2,1}
```

Another Lua library function where lambdas are useful is `string.gsub`. The problem is that some log formats just use unadorned UNIX time-stamps, but we turn this into an opportunity to present the data exactly as we want:

```sh
$ el ^'%d,%s' time[] ^'A log message' > log.log
$ cat log.log
1636908445,A log message
$ cat log.log | el gsub it ^'%d+' {t: date ^'%F %T' t}
2021-11-14 18:47:25,A log message
```

Higher-order functions can be concisely expressed:

```sh
$ el set f={y: {x: x+y}}
$ el f[10][20]
30
$ el set tstamp={fmt: {t: date fmt t}}
$ cat log.log | el gsub it ^'%d+' tstamp[^'%F %T']
2021-11-14 18:47:25,A log message
```

## Filtering Text

Filters are an important part of the shell experience, and I wanted `el` to be useful in shell pipelines.

```
$ cat text.txt
Normally text isn't as interesting
as a line from a poem,
or a sentence scrawled in lipstick
on the bathroom mirror
$ cat text.txt | el lines do sub it 1 6
Normal
as a l
or a s
on the
```
There's no magic here: `io.lines()` and `string.sub()`, in their informal, first-names-please guise. There is a command `cut` that does this kind of thing, but I can never remember it, whereas I do remember Lua standard library functions.

There is another special variable, `row`, which increments from 1 to n over the input:

```sh
$ cat text.txt | el lines do ^'%02d %s' row it
01 Normally text isn't as interesting
02 as a line from a poem,
03 or a sentence scrawled in lipstick
04 on the bathroom mirror
```

`lines do` is common to all filters over input lines, so there is yet another implicit; if the expression contains that special variable `it` then the loop is over all lines:

```sh
$ cat text.txt | el sub it 0 6
Normal
as a l
or a s
on the
```
There is a postfix `if`, allowing yet another re-invention of `grep` (note the double '^^')

```sh
$ cat text.txt | el it if match it ^^or
or a sentence scrawled in lipstick
```
But this is _an expression_, so we can make more involved queries without going blind trying to write complicated regexes:

```sh
$ cat text.txt | el it if {match it ^a} and {match it ^poem}
as a line from a poem,
```
(My tests currently involve a one-liner to collect all the commands in this file:
```sh
cat readme.md | el match it ^'^%$ (.+)' > test.sh
```
)

## Too Much Implicit?

This started as a calculator-itch and ended up scratching something more interesting: mostly (as an over-generalization) shells are nasty programming languages and vice versa. So we take Lua and make it easier to write once-off expressions. The coding styles appropriate for applications (e.g. 'global-free' for Lua) doesn't apply in console golfing. 'Type-ability' is a thing, and the punctuation in 'match("hello dolly","^hell")' slows us down and seems tiring. (At least subjectively, and that is sufficient for me now). In the context of programming, typing is a not an important part of total effort, but shells are about typing fast and accurately.

There are two kinds of implicits going on in `el` - one is eliding common functions in context, such as `seq`, `format` and `list`. The other is where we look for a particular, special var `it` and assume it must be looped (which is very AWK-ish, another favourite tool of mine). There is an implicit table constructor where a key-value argument makes the rest of the arguments collected as a table. That could be a little surprising, I will grant you. But these are just experiments in a certain design space - it is hard to evaluate a feature without an implementation, and it is certainly no fun. I grant you that I am laying on the special sauce a little thick sometimes, but it is all optional.

The implementation itself is a fevered hot mess of string patterns and substitutions, but the great thing about Itch Research is that a bad implementation is sufficient to demonstrate ideas enough to show whether they are bad, good or merely meh. There is no point in spending weeks doing a formal grammar and parser if the idea itself is not useful to more than a handful of people. But it means that things are rough and the error handling is poor.

## Appendix I: Useful Global Functions Available

We inject all the functions available in `math,io,os,string,bit32,table`, but some functions are adjusted. A chain of expressions is better than a seriously nested set of function invocations.

Note that the format string passed to `format` and `printf` is specially massaged: '%rs' means 'print this string in red' and '%b00d` is 'print this integer with blue'. These are available `{r=red, g=green, y=yellow, b=blue, m=magenta, c=cyan, w=white}`. (This of course needs support for ANSI codes, which even _Windows_ does these days). There is a global `paint` which is a paint factory: `paint.r` is a function that colours its argument.

  - `len` is redefined in terms of `#t`
  - `gsub` returns only the string
  - `insert` also sets `auto_save`
  - `sort` returns the table
  - `write` sets `print_newline` flag so `el` ends with a newline
  - `append` like `insert` except _multiple_ values can be pushed, and `auto_save` is set
  - `first` return the non-whitespace start of a string (replaces the clunky `awk '{print $1}'`)
  - `printf` just `write(format(fmt,...))`
  - `json` render a value as JSON
  - `lua` render a value in Lua format
  - `exec` collect arguments together and run as shell. Any map-like key-value pairs become flags, and if the value is an array, it represents multiple flags.
  - `get`,`post` wrappers around `curl` that use `exec`
  
  
```sh
$ el set T={}
$ el append T 10 : sort it
{10}
$ el append T 2 : sort it
{2,10}
$ el append T 15 : sort it
{2,10,15}
$ el exec^ echo 'hello world'
hello world
```

The `curl` wrappers exploit `el`'s table constructors. Here we use the excellent `httpbin` to test our queries. In the first case to save us from writing out query vars (and *escaping* them) and second to pass up some JSON to a server directly:

```sh
scratch$ export URL=https://httpbin.org
scratch$ el get query={a=23 hello=^dolly} ^/anything
{
  "args": {
    "a": "23", 
    "hello": "dolly"
  }, 
  "data": "", 
  "files": {}, 
  "form": {}, 
  "headers": {
    "Accept": "*/*", 
    "Host": "httpbin.org", 
    "User-Agent": "curl/7.58.0", 
    "X-Amzn-Trace-Id": "Root=1-6195f3b3-31aa5b0945538df91eb19a2d"
  }, 
  "json": null, 
  "method": "GET", 
  "origin": "197.91.187.145", 
  "url": "https://httpbin.org/anything?hello=dolly&a=23"
}

scratch$ el post data={one=1 two=2} headers={one=^first two=^second} ^/anything 
{
  "args": {}, 
  "data": "{\"one\":1,\"two\":2}", 
  "files": {}, 
  "form": {}, 
  "headers": {
    "Accept": "*/*", 
    "Content-Length": "17", 
    "Content-Type": "application/json", 
    "Host": "httpbin.org", 
    "One": "first", 
    "Two": "second", 
    "User-Agent": "curl/7.58.0", 
    "X-Amzn-Trace-Id": "Root=1-61960805-282af85d44fb514577e47615"
  }, 
  "json": {
    "one": 1, 
    "two": 2
  }, 
  "method": "POST", 
  "origin": "197.91.187.145", 
  "url": "https://httpbin.org/anything"
}
```
  
There are some useful iterators:
  - `seq` i1,i2,inc - `inc` defaults to 1, and if no `i2` the range is 1 .. `i1`
  - `iter` t returns each value of a table sequence
  - `items` takes an arbitrary number of arguments and iterates over them
  - `spliti` s,re  - parts of string separated by delim
  
Conversions:
  - `bin` renders as binary, least signicant bit first
  - `hex` renders as hexadecimal
  
Generally useful:
  - `add`,`mul` and `cat` are n-ary functions (for when using the operators is tedious)
  - `slice` t,i1,i2 makes a copy of a range of an array
  - `index` t,val - index of val in the array
  - `split` s,re - parts of string separated by delim
  - `map` t,f apply the function `f` and create a new table. Only non-nil values so this is a _filter map_
  - `zipmap` t1,t2(,f) zip two tables together. If `f` is provided, use that instead of `{a,b: {a,b}}`
  - `fields` p.cols,p.delim,p.pat - this parses delimited fields and constructs a table

Some examples. Here we concatenate some items together (`add` and `mul` are useful here!). Then some array operations, using `:` to build up a chain of operations:

```sh
$ el zipmap {1 2 3} {^ one two three} cat
{"1one","2two","3three"}
$ el T
{"one","two","three","four"}
$ el index T ^three : slice T it : map it upper
{"THREE","FOUR"}

```

Here is a `which`, which is made clearer with a few functions:

```sh
$ el split PATH ^: : map it {p: open['p.."/go"'] and p}
{"/home/steve/sdk/go1.16.3/bin"}
$ el set join={p1,p2: p1 .. ^'/' .. p2}
$ el set exists={p: open[p] and p}
$ el split PATH ^: : map it {p: exists join[p,^go]}
{"/home/steve/sdk/go1.16.3/bin/go"}
```
The result is a table; this is more elegant:

```sh
$ el spliti PATH ^: do exists {join it ^go}
/home/steve/sdk/go1.16.3/bin/go
```
It goes beyond the usual `which` since it will find all occurances, not the first. Sometimes you really don't wish to go any further through some enormous file:

```sh
$ cat text.txt | el stop it if row==2
as a line from a poem,
```

`stop` prints out its value and exits with zero, if the value is truthy (not nil or false). It is considered an output conversion so we get the nice flat statement sugar. It is but an experiment; I suspect there is a more general mechanism lurking behind it.

`fields` is useful for processing structured data like CSV. Note that it is very convenient
to use the table-as-argument trick to get named parameters. 

```sh
$ echo '10,20,30' | el fields cols=^'x,y,z' delim=^, it
{x=10,y=20,z=30}
$ echo '10,20,30' | el fields cols=^'x,y,z' delim=^, it : it.x*it.y
200
$ echo 'hello(dolly)' | el fields pat=^'(%a+)%((%a+)%)' cols=^'greeting,name' it
{greeting="hello",name="dolly"}
```

## Appendix II Some Trickery Used to Prepare this Entertainment

### Metatable Madness

Global lookup proceeds by looking at math,io,os,string,bit32,table and then `saved_globals`, which is the table containing all the key-values stored by `set`. Then we check if it looks like a binary 'literal', e.g 'b1011' and then if we can look it up in the environment. Obviously this is not the fastest way to do this but good enough for now (in dynamic languages you pay for cleverness at runtime, not compile-time)

So, how is `saved_globals` managed?

```sh
scratch$ cat ~/.el
local saved = {_G=_G};_ENV=saved
simple={two=2,one=1}
_LITERAL_={join="function(p1,p2) return p1 .. '/' .. p2 end",f="function(y) return function(x) return x+y  end  end",tstamp="function(fmt) return function(t) return date(fmt,t) end  end",exists="function(p) return open(p) and p end",desc="function(a,b) return a > b end"}
_ENV=saved._G
join=function(p1,p2) return p1 .. '/' .. p2 end
f=function(y) return function(x) return x+y  end  end
tstamp=function(fmt) return function(t) return date(fmt,t) end  end
exists=function(p) return open(p) and p end
desc=function(a,b) return a > b end
return saved
```
For regular data values, things are straightforward: they get put into `saved` using the `_ENV` strategy. But we put the actual functions into `_G`, and keep their code representation in the `_LITERAL_` table. Next time `set` gets called, the values will be separated into data and functions, and the values go after the `_ENV=saved`, and the functions get copied into `_LITERAL_`.

A similar trick is used for `add_file` (uses `dofile('/path/to/file')`) and `add_mod` (uses `mod=require('mod`)`)

A hack was required to make functions work - we automatically _quote them as strings_ in a key-value context.

Saving/loading is the platform-dependent part here, should not be difficult to generalize. A sensible person might ask: can you not just add some dependencies? But nah: single-file Lua is the way to make things easier for users, although it might offend our fine-tuned developer feelings.

### Massage for Shell Happiness

The standard shell uses a *lot* of characters and so we have to adjust our lexical symbols to what will not give offense. `()` are special, so functions are called by `f[x]` (or `f x` if the stars are in alignment). Since `'"hello"'` is awkward to type, `el` has the quote marker `^hello`.

Things like `>` and `|` must be quoted, but at least only single quotes are needed. `*` is a sneaky one, since if the shell *can* expand it, it will, so `2 * 3` blows up weirdly but `2*3` will not.

All decisions have consequences which may cause headaches. Array indexing is messed because `a[1]` becomes `a(1)` and arrays are (generally) not callable. So, we *make* them callable, either as a result of `list` or when loaded, with a metatable that implements call in terms of indexing. This is not beautiful but (again) will do for now. (There's an opportunity to invent slice notation `a[2,3]` here but perhaps for another day)

### Parsing Kludgery

The moral of the story, don't just start typing. But sometimes, that's how you start. The parser has far too many 'if ... but' statements and so the features tend to fight with each other. The implicit stuff is mostly optional, but implicit lists will bite:

```sh
$ el {10 + 20}
error	[string "expr"]:1: unexpected symbol near '+'	printx(eval(list(10,+,20)))
$ el {eval 10 + 20}
30
```
So `el` solves the old Lisp quoting problem the other way around: it assumes subexpressions are lists, unless the first item is a *global function*. This is convenient but (again) will bite us further down the road - it is not possible to understand from the *shape* the code how it will be interpreted.

I think the most scary kludge was to get string-quoting functions to behave as desired, i.e. let tables through:

```sh
$ el json^ a=hello b='you peeps' c={10 ^20}
{"b":"you peeps","c":[10,"20"],"a":"hello"}
```
To do this, subexpressions had to be marked using a prefixed `\01` byte (!). Such things happen when all you have are strings.


