# Discovering Haskell while creating a repl DB client for barrelDB


## introduction

A year ago I created a POC for a repl client for the interesting barrelDB.
It was a pet project, and gave me the opportunity to explore the basics of Haskell.
A year after I made a kind of `ok but not great talk` about functionnal programming in `C++`.
It seems like my interrest for functionnal programing stayed.
I met the passionate @benoic for making this POC a real thing.
After our discution I identified 7 subjects to be addressed.

3 mandatory :
- Repl engine                   : interract with the shell
- Command line parser           : parse writen sentences to usable haskell objects
- Transport library             : sends and recieve

4 must have :
- Document reader               : read and validate input documents before sending them
- Concurency                    : allow several commands to run at the same time
- Stream processing             : handle the case of stream
- Post processing operations    : give the ability to do operations on the recieved documents

## Where do I begin ?

To answers this question I used the dependency scheme : What can I do without needing the rest.

In the mandatory list I have only one candidate the `Transport library`.

It's needs a command as input.

```
create_db
{
    "name" : "my_db"
}
```

And and output

```
{
    "result" : "ok"
}
```

The `Repl engine` needs the command line parser to form commands from input strings.

The `Command line parse` needs to send real commands to be developped a pragmatic way.

## Transport library

### Getting inspired

`barrelDB` has similarities with `couchDB` and there is an haskell implementation for [it](http://hackage.haskell.org/package/CouchDB-1.2.2/docs/Database-CouchDB.html)

The part concerning the transport layer is [here](hackage.haskell.org/package/CouchDB-1.2.2/docs/src/Database-CouchDB-HTTP.html).

It consists of a monad based encapsulating the IO monad and a connection.


## Haskell is `nerd` fun

### The smell of lisp

Haskell has reminds me the time I studied lisp in the IA course of professor [Tupia](https://www.pucp.edu.pe/profesor/manuel-tupia-anticona) in the PUCP.


Lets have a look a it in ghci see this [link](https://www.haskell.org/platform/) to install haskell.


```bash
ghci
λ> ( 3 + 4 ) / ( 4 + 6 )
0.7
```

This simple calculation is using `infix` operators. It's an instinctive thing for most of us.

Those are all equivalent :

```haskell
( 3 + 4 ) / ( 4 + 6 )
(/) ( 3 + 4 ) ( 4 + 6 )
(/) ( (+) 3 4 ) ( (+) 4 6 )
```

Let's have a look at lisp version :

```bash
clisp
[1]> (/ (+ 3 4) (+ 4 6))
7/10
```

It's one of the many reasons why `lisp`and `haskell` community are connected.


### Equivalents things are beautifull

Haskell has some operators which I found really from a graphical point of view

### The case of `$`

Consider the following operation :

```bash
ghci
λ> 1000 / ( 100 / ( 5 / 2 ))
25.0
```

Here parentesis are clearly needed :

```bash
λ> 1000 / 100 / 5 / 2
1.0
```

Let me introduce `$`. It's an operator to use less parenthesis, it basicaly wraps what is after it with parentesis.


Let's write it lisp's way

```bash
λ> (/) 1000 ( (/) 100  ( (/) 5  2 ))
25.0
```

With `$` you can simplify the writing :

`$` once :

```bash
λ> (/) 1000 ( (/) 100  $ (/) 5  2 )
25.0
```

Notice that in this example `$` gets applied before the parentesis.

`$` twice :

```bash
λ> (/) 1000 $ (/) 100  $ (/) 5  2
25.0
```

It seams like a toy, but it's actualy very usefull in `haskell`.


## references

[`infix functions`](https://wuciawe.github.io/functional%20programming/haskell/2016/07/03/infix-functions-in-haskell.html)
