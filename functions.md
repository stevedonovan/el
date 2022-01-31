# Useful Global Functions Available

We inject all the functions available in `math,io,os,string,bit32,table`, but some functions are adjusted. A chain of expressions is better than a seriously nested set of function invocations.

Note that the format string passed to `format` and `printf` is specially massaged: '%rs' means 'print this string in red' and '%b00d' is 'print this integer with blue'. These colours are available `{r=red, g=green, y=yellow, b=blue, m=magenta, c=cyan, w=white}`. (This of course needs support for ANSI codes, which even _Windows_ does these days). There is a global `paint` which is a paint factory: `paint.r` is a function that colours its argument.

  - `len` is redefined in terms of `#t`; `nil` has a length of 0
  - `match` allows some convenient shortcuts:
  
```
local simple_patterns = {WORD = '(%S+)', DIGIT = '(%d+)', REST = '(.+)',
    IDEN = '(%a[%w_]*)', SSTR = "'(.-)'", DSTR = '"(.-)"', LAZY = '(.-)'}
```
Can switch these off with `EL_NO_PAT_MASSAGE`.
  
  - `gsub` returns only the string, also uses the shortcuts
  - `sort` returns the table. There is some optional magic - can provide a lambda,but if the items are tables, there's a shortcut for sorting on a field name or index, plus a direction.
  
```
$ el sort { {name=^bob age=34} {name=^alice age=35}} ^name
{{age=35,name="alice"},{age=34,name="bob"}}
# sort descending! gt is the '>' operation
$ el sort { {name=^bob age=34} {name=^aallice age=35}} ^age gt
{{age=35,name="alice"},{age=34,name="bob"}}
```
  - `write` sets `print_newline` flag so `el` ends with a newline
  
There are some useful constants: `space,lf,tab,empty_string = ' ','\n','\t',''` so can say

```
$ el write 42 space ^everything
42 everything
```

  - `empty` `empty(v)` is true if `len(v)==0`
  - `append` like `insert` except _multiple_ values can be pushed, and `auto_save` is set
  - `extend` extends the first table with the second table
  - `push`  semi-magical insert used at the end of loops
  - `first` return the non-whitespace start of a string (replaces the clunky `awk '{print $1}'`)
  - `printf` just `write(format(fmt,...))`
  - `json` render a value as JSON
  - `lua` render a value in Lua format
  
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

There are some useful iterators:
  - `seq` i1,i2,inc - `inc` defaults to 1, and if no `i2` the range is 1 .. `i1`
  - `iter` t returns each value of a table sequence
  - `items` takes an arbitrary number of arguments and iterates over them
  - `spliti` s,re  - parts of string separated by delim
  
Conversions:
  - `bin` renders as binary, least signicant bit first
  - `hex` renders as hexadecimal
  - `put` writes out as usual, but returns the value - useful for printing intermediate results
  
Generally useful:
  - `add`,`mul` and `cat` are n-ary functions (for when using the operators is tedious)
  - `vars` is a useful do-nothing function: `el vars x=10 y=2 : it.x*it.y`
  - `glob` creates global variables `el glob x=10 y=2 : x*y`
  - `let` is an alias for `glob`
  - `update` updates key-values for a table  `el let T={a={b=1}} : update T ^a.b 10`
  - `read_num` reads a single number from standard input
  - `line` reads a given line from stdin
  - `slice` t,i1,i2 makes a copy of a range of an array
  - `index` t,val - index of val in the array
  - `index_by` t,ii - result is `{t[i]}` for all `i` in `ii`
  - `collect` iter - collect an iterator into an array
  - `split` s,re - parts of string separated by delim
  - `map` t,f apply the function `f` and create a new table. Only non-nil values so this is a _filter map_
  - `mapma` apply the function to a map-like table and return an array-like table
  - `mapam` apply the function to an array-like table and return a map-like table
  - `zip` t1,t2(,f) zip two tables together. If `f` is provided, use that instead of `{a,b: {a,b}}`
  - `filter` only keep values that match a predicate
  - `fields` p.cols,p.delim,p.pat - this parses delimited fields and constructs a table

Some examples. Here we concatenate some items together (`add` and `mul` are useful here!).
Note that `..` is defined on sequence-like tables! 
Then some array operations, using `:` to build up a chain of operations:

```sh
$ el zip {1 2 3} {10 20 30} : json it
[[1,10],[2,20],[3,30]]
$ el zip {1 2 3} {10 20 30} {a,b: x=a y=b} : json it
[{"y":10,"x":1},{"y":20,"x":2},{"y":30,"x":3}]
$ el zip {1 2 3} {^ one two three} cat
{"1one","2two","3three"}
$ el cat {1 2 3} {4 5 6} {7 8 9} : sort it gt
{9,8,7,6,5,4,3,2,1}
$ el set T={^ one two three four}
$ el index T ^three : slice T it : map it upper
{"THREE","FOUR"}
```

The idea of mapping gets more interesting when generalized to both array-like and map-like tables. (Lua does not force that binary choice, but we use them as either one or the other)

`mapma` goes from a map to an array, and `mapam` goes from an array to a map. They have default functions and then work as inverses of each other:

```
$ el mapma {a=1 b=2}
{{"b",2},{"a",1}}
$ el mapma {a=1 b=2} : mapam it
{a=1,b=2}
$ el zip {^ a b} {10 20} : mapam it
{a=10,b=20}
```

All the map/filter functions take an optional extra argument that will be passed to the function.

```
$ el map {10 20 30} add 1
{11,21,31}
$ el filter {34 0 43 2 0} gt 0
{34,43,2}
```

Iterators can be collected into arrays. Of course, the simplest form of an iterator is just a function that returns non-nil, and (hopefully) returns `nil` eventually. So to make an iterator function, make a function that returns such a function. A bit awkward to type so this operation is given the vaguely exciting name `fun`.

```sh
$ el collect {seq 0 1 0.1}
{0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0}
$ echo 10 20 30 | el collect read_num
{10,20,30}
$ echo 10 20 30 | el {: read_num} do it
10
20
30
$ echo 10 20 30 | el fun read_num do it
10
20
30
```

Here is a `which`, which is made clearer with using `exists`:

```sh
$ el split PATH ^: : map it {p: open['p.."/go"'] and p}
{"/home/steve/sdk/go1.16.3/bin"}
$ el split PATH ^: : map it {p: exists p ^go}
{"/home/steve/sdk/go1.16.3/bin/go"}
```
The result is a table; this is more elegant:

```sh
$ el spliti PATH ^: do exists it ^go
/home/steve/sdk/go1.16.3/bin/go
```
It goes beyond the usual `which` since it will find all occurances, not the first.

Sometimes you really don't wish to go any further through some enormous file:

```sh
$ cat data/text.txt | el stop it if row==2
as a line from a poem,
```

`stop` prints out its value and exits with zero, if the value is truthy (not nil or false). It is considered an output conversion so we get the nice flat statement sugar. It is but an experiment; I suspect there is a more general mechanism lurking behind it.

`fields` is useful for processing structured data like CSV. Note that it is very convenient
to use the table-as-argument trick to get named parameters. 
Values will be converted to numbers if possible.

```sh
$ echo '10,20,30' | el fields cols=^'x,y,z' delim=^, L
{x=10,y=20,z=30}
$ echo '10,20,30' | el fields cols=^'x,y,z' delim=^, L : it.x*it.y
200
$ echo 'hello(dolly)' | el fields pat=^'(%a+)%((%a+)%)' cols=^'greeting,name' L
{greeting="hello",name="dolly"}
```
If you don't provide `cols` you will get an array. With `gfields`
it sets global field variables like AWK:

```sh
$ echo '10 20' | el fields L
{10,20}
$ echo '10 20' | el gfields L : f1 f2 f1/f2
10      20     0.5
```

Sometimes we need to perform an operation on a particular line of input:

```sh
$ ifconfig wlo1 | el line 2 : match it ^'inet WORD'
192.168.1.67
```
`line` gives you the given line of standard input - the default is the first line.

Can continue to do this:

```sh
$ cat data/text.txt | el line 2 : put match it ^'line WORD' : line 3 : match it ^'sentence WORD'
from
scrawled
```
This is a cool idiom for doing multi-line matches.

However, it's not easy to *collect* the results. For that, `mmatch`. The problem is that `ip addr` produces appalling output, and we want to collect the names and IP addresses of the devices.

```sh
$ ip addr | el mmatch L ^'DIGIT: WORD:' ^'inet6* WORD?'
{"1","lo","127.0.0.1/8"}
{"2","enp0s25"}
{"3","wlo1","192.168.1.67/24"}
{"4","br-0efaa7b4f508","172.20.0.1/16"}
{"5","docker0","172.17.0.1/16"}
{"6","br-a4f048baa9de","172.19.0.1/16"}
{"7","br-de7fdfccc0e7","172.18.0.1/16"}
```

So, we first match '3: wlo1' and get the name, and we start matching with the next pattern, which picks up the `inet 192.168.1.67/24` a few lines down - and then we're finished.

But no IP address for 'enp0s25'? Well, it's not connected to anything. This is why there is a _mysterious question mark_ at the end of the second pattern. Think of it as a simple higher-level pattern `ab?` where `a` and `b` are Lua string patterns. We abandon the search for 'enp0s25' since we find '3: wlo1:'. 

Want result as nice JSON objects? 

```sh
$ ip addr | el mmatch L ^'DIGIT: WORD:' ^'inet6* WORD?' : json fields it cols=^'no,dev,ip'
{"ip":"127.0.0.1/8","dev":"lo","no":1}
{"no":2,"dev":"enp0s25"}
{"ip":"192.168.1.67/24","dev":"wlo1","no":3}
{"ip":"172.20.0.1/16","dev":"br-0efaa7b4f508","no":4}
{"ip":"172.17.0.1/16","dev":"docker0","no":5}
{"ip":"172.19.0.1/16","dev":"br-a4f048baa9de","no":6}
{"ip":"172.18.0.1/16","dev":"br-de7fdfccc0e7","no":7} 
```
`fields` will work with an *existing* array and make up an object using column names.

Another example of this useful pattern for parsing commands into structured data:

```sh
$ ping -c 2 google.com | el mmatch L ^'PING LAZY %(LAZY%)' ^'= LAZY/LAZY/LAZY/LAZY ms' : fields it cols=^'host,ip,min,avg,max,mdev'
{max=7.266,ip="172.217.170.46",host="google.com",min=5.91,mdev=0.678,avg=6.588}
```

Consider the task of counting how many if-statements there are in each global function in `el` itself. `mmatch` can do it with the `+` qualifier:

```sh
$ cat el.lua | el let funs={} : lines do mmatch it ^'^function IDEN' ^if+ ^^end
{"squote","end"}
{"maparg",{"if"},"end"}
{"global_lookup",{"if","if","if"},"end"}
{"set_autosave","end"}
{"set_global_lookup",{"if","if"},"end"}
{"mmatch",{"if","if","if","if","if","if","if","if","if","if","if","if","if"},"end"}
{"len",{"if"},"end"}
...
```
That is, 'if' may match _multiple times_. Certainly can now eyeball that `mmatch` itself is complex, judging by the captured 'if's - note how they appear in their own table. But we want numbers, generally: `' : update it 2 len` will turn the array of ifs into their length. Can collect these arrays into a array, and then sort and slice to get the top 10 complicated functions:

```sh
$ cat el.lua | el let funs={} : lines do mmatch it ^'^function IDEN' ^if+ ^^end : update it 2 len : insert funs it end sort funs 2 gt : slice it 1 5
{{"subexpr",30,"end"},{"chain_expr",25,"end"},{"save",17,"end"},{"mmatch",13,"end"},{"main",12,"end"}}
```
`el` lines can get a bit long!

Here is a more idiomatic solution:

```sh
$ cat el.lua | el mmatch L ^'^function IDEN' ^if+ ^^end : update it 2 len : push it end sort it 2 gt : slice it 1 5

```
That `push it end sort it...` is the easiest way to collect an array of results for processing after the loop end.

Can make it pretty with `tablefy`: note the `3 nil` to get rid of the last column of data.

```sh
$ cat el.lua | el  mmatch L ^'^function IDEN' ^if+ ^^end : update it 2 len 3 nil : push it end sort it 2 gt : slice it 1 5 : tablefy it left=2 top=1 bottom=1

  subexpr             30  
  chain_expr          25  
  save                17  
  mmatch              13  
  collect_subexprs    12  

```

Here is another prettification - annotating verses with their line number. `nseq` is a function that returns a consecutive integer. If any row is not a table, then `tablefy` assumes it is a blank line.

```sh
$ cat data/justtosay.txt | el not empty[L] and {L nseq[]} or 0 : push it end tablefy it
I have eaten         1   
the plums            2   
that were in         3   
the icebox           4   
                         
and which            5   
you were probably    6   
saving               7   
for breakfast        8   
                         
Forgive me           9   
they were delicious  10  
so sweet             11  
and so cold          12  
```

## Extra Functions

These are not in the core (mostly for the sensible reason that people are uncomfortable with source files longer than 1500 lines) but can be very useful for specific tasks

  - `exec()` collect arguments together and run as shell. Any map-like key-value pairs become flags, and if the value is an array, it represents multiple flags.
  - `curl()` frontend for `curl` that uses `exec`

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
### Miscelaneous

  - `subst` do a `gsub` on `${var}`
  - `patch` map a range in a string
  
```sh
$ echo 'hello ${dolly}' | el subst @stdin {dolly=^fine}
hello fine

$ el patch ^'hello dolly' 1 3 upper
HELlo dolly

```
