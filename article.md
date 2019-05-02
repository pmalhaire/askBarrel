# A road trip in real world haskell : askBarrel a client for barrelDB

1. [Introduction](#introduction)
2. [Where do I begin ?](#where-do-i-begin-)
   1. [Let's take an example](#lets-take-an-example)
   2. [reduce the problem](#reduce-the-problem)
   3. [And the beginner is ... `Transport library`](#and-the-beginner-is--transport-library)
3. [Let's enter the Haskell world](#lets-enter-the-haskell-world)
   1. [Haskell is `nerd` fun.](#haskell-is-nerd-fun)
   2. [The smell of lisp](#the-smell-of-lisp)
   3. [Meet the haskell syntax](#meet-the-haskell-syntax)
      1. [The case of `$`](#the-case-of-)
4. [The monad universe](#the-monad-universe)
   1. [A big picture](#a-big-picture)
      1. [functor](#functor)
         1. [technical definition](#technical-definition)
         2. [practical example](#practical-example)
      2. [applicative](#applicative)
         1. [technical definition](#technical-definition-1)
         2. [practical example](#practical-example-1)
      3. [Monad](#monad)
         1. [technical definition](#technical-definition-2)
         2. [practical example](#practical-example-2)
   2. [All programs begin with the `IO monad`](#all-programs-begin-with-the-io-monad)
      1. [Huston there is a problem](#huston-there-is-a-problem)
      2. [Interact with the shell](#interact-with-the-shell)
5. [Misc](#misc)
6. [Transport library](#transport-library)
   1. [Getting inspired](#getting-inspired)


## Introduction

A year ago I created a POC for a repl client for the interesting barrelDB.
It was a pet project, and gave me the opportunity to explore Haskell basics.
A year after I made a kind of `ok but not great talk` about functional programming in `C++`.
I learned a lot doing it seems like my interest for functional programing stayed.
I met @benoic the author of barrelDB which encourage me to make this POC a real thing.
We discussed and our discussion I identified subjects to be addressed.

mandatory :
1. Repl engine                   : interact with the shell
2. Command line parser           : parse written sentences to commands
3. Query parser                  : parse DB queries
4. Transport library             : sends and receive data from the DB server

must have :
1. Document reader               : read and validate input documents before sending them
2. Concurrency                   : allow several commands to run at the same time
3. Stream processing             : handle the case of stream
4. Post processing operations    : give the ability to do operations on the received documents

## Where do I begin ?

### Let's take an example

Let's make things more concrete. Let's create a db.


The `Repl engine` feeds the `Command line parser` with input strings :

```bash
barrelDB >>> create_db { "name" : "my_db" }
```

The `Command line parser` cuts the command in parts to feed the `Query parser`

| command | data |
|---|---|
|create_db | { "name" : "my_db" } |

The `Query parser` check the validity of the data given to the command and gives a buffer to the `Transport library`.

| command  | check | status |
|---|---|---|
| create_db | has a name  |	&#10003; |
| create_db | has name is not empty  |	&#10003; |


The `Transport library` sends the query buffer to the `DB`
```bash
┌────────┬─────────────────────────┬─────────────────────────┬────────┬────────┐
│00000000│ 7b 20 6e 61 6d 65 20 3a ┊ 20 6d 79 5f 64 62 20 7d │{ name :┊ my_db }│
│00000010│ 0a                      ┊                         │_       ┊        │
└────────┴─────────────────────────┴─────────────────────────┴────────┴────────┘

```

The `Transport library` receives the response buffer from the `DB` and the it to the `Command line parser`

```bash
┌────────┬─────────────────────────┬─────────────────────────┬────────┬────────┐
│00000000│ 7b 20 72 65 73 75 6c 74 ┊ 20 3a 20 6f 6b 20 7d 0a │{ result┊ : ok }_│
└────────┴─────────────────────────┴─────────────────────────┴────────┴────────┘

```

The `Command line parser` get the gives the response to the `Repl engine`

| message | data |
|---|---|
| response | `{ "result" : "ok" }` |

The `Repl engine` write the response to the shell

```bash
{ "result" : "ok" }
```

<!-- butchery analogy -->

### reduce the problem

<!-- my brain (small) my problem (big) -->

Let's write the steps from our example.

1. get query `q` from the user
2. connect to the DB
3. send `q` to the DB server
4. get response `r` from the DB server
5. write `r` to the shell
6. close the program


After an intense usage agile no bullshit disruptive methodology.
I faced to a terrible conclusion.

If I can't talk to the `DB server` this project is dead.

<!-- muted crossed scotch head in a form of a DB -->

### And the beginner is ... `Transport library`

To simulate my DB I'll use create_db command :

Input:

```json
{
  "name" : "my_db"
}
```

Possible outputs:

```json
{
  "result" : "ok"
}
```

```json
{
  "result" : "fail"
}
```

## Let's enter the Haskell world

### Haskell is `nerd` fun.

When you learn new languages it's practical to use analogies from one language to an other. In haskell you can't unless you've studied an other functional language. For a curious person sad that nodeJS took the world with it's ugly code this make things exiting. I learn something new each time I code in Haskell.

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


### Meet the haskell syntax

Haskell is a rich universe with many entrance doors.

I'll give you mine which is somehow singular : Haskell syntax is graphically beautiful.

#### The case of `$`

Consider the following operation :

```bash
ghci
λ> 1000 / ( 100 / ( 5 / 2 ))
25.0
```

Here parenthesis are clearly needed :

```bash
λ> 1000 / 100 / 5 / 2
1.0
```

Let me introduce `$`. It's an operator to use less parenthesis, it wraps what is after it with parenthesis.


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

Notice that in this example `$` gets applied before the parenthesis.

`$` twice :

```bash
λ> (/) 1000 $ (/) 100  $ (/) 5  2
25.0
```

It seams like a toy, but it's very useful in `haskell`.


For more details see [`infix functions`](https://wuciawe.github.io/functional%20programming/haskell/2016/07/03/infix-functions-in-haskell.html).

## The monad universe

Monad has became a mythical creature in programming concepts universe. I used it to look smart when I did not understand it at all. (I wonder if I lied to others or to myself when I said I knew what monad were)

I won't lie, it's not a pain free to learn it. And to be honest I am not entirely comfortable with it. But it's conceptually interesting and helped me to approach many concepts I didn't even note before.

I'll expose one by one the concepts behind monads. Your brain will slowly be surely assemble the puzzle. You'll fell fireworks in your brain. Don't panic it's normal.

### A big picture

<!-- draw inclusion of concepts -->
Illustration:

(Monad (applicative (functor)))


#### functor

A functor is a box with which :
- contains zero or more instances `i1, ... in` of a type `a`
- can apply a function from type `a` to type `b` on each of `i1, ... in` resulting on `u1, ... un`.


##### technical definition

```haskell
Prelude> :i Functor
class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  {-# MINIMAL fmap #-}
  	-- Defined in ‘GHC.Base’
instance Functor (Either a) -- Defined in ‘Data.Either’
instance Functor [] -- Defined in ‘GHC.Base’
instance Functor Maybe -- Defined in ‘GHC.Base’
instance Functor IO -- Defined in ‘GHC.Base’
instance Functor ((->) r) -- Defined in ‘GHC.Base’
instance Functor ((,) a) -- Defined in ‘GHC.Base’
```

##### practical example

example : [] is a functor


Apply a function

```haskell
Prelude> fmap (+1) [1, 2]
[2, 3]
```

```haskell
Prelude> fmap (==1) [1,2]
[True,False]
```

Replace the content

```haskell
Prelude> 5 <$ [1]
[5]
```

#### applicative

An applicative `A` is an `enhanced` functor which :
- has a function from `a` to `A` ( kind of from `a` to `boxed` `a` )
- has a function
    from :
        1 : functions from type `a` to type `b` `boxed` in the instance A1 of `A`
        2 : values of type `a` `boxed` in the instance A2 of `A`
    to :
        values of type `b` `boxed` in the instance A3 of `A`

##### technical definition

```haskell
Prelude> :i Applicative
class Functor f => Applicative (f :: * -> *) where
  pu:re :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  GHC.Base.liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  (*>) :: f a -> f b -> f b
  (<*) :: f a -> f b -> f a
  {-# MINIMAL pure, ((<*>) | liftA2) #-}
  	-- Defined in ‘GHC.Base’
instance Applicative (Either e) -- Defined in ‘Data.Either’
instance Applicative [] -- Defined in ‘GHC.Base’
instance Applicative Maybe -- Defined in ‘GHC.Base’
instance Applicative IO -- Defined in ‘GHC.Base’
instance Applicative ((->) a) -- Defined in ‘GHC.Base’
instance Monoid a => Applicative ((,) a) -- Defined in ‘GHC.Base’
```

##### practical example

example : Maybe is a an applicative it's used to have a `simple` box which is empty or contain a value. A list of one or zero value.

an empty `Maybe` value is `Nothing`.

```haskell
Prelude> Nothing
Nothing
Prelude> :t Nothing
Nothing :: Maybe a
```


`boxed` `A` with Maybe

```haskell
Prelude> Just 1
Just 1
Prelude> :t Just 1
Just 1 :: Num a => Maybe a
Prelude>
```


An applicative usage : compare inside the `box`

```haskell
Prelude> (==) <$> (Just 1) <*> (Just 2)
Just False
```

An applicative usage : test inside the `box`

```haskell
Prelude> [ (>2)] <*> [1,2,3]
[False,False,True]
```


```haskell
Prelude> [(>2),(<3)] <*> [1,2,3]
[False,False,True,True,True,False]
```

an other way using liftA2 notice that the operation is applied over all possibilities between the lists.

```haskell
Prelude Control.Applicative> :m Control.Applicative
Prelude Control.Applicative> liftA2 (+) [3] [4]
[7]
Prelude Control.Applicative> liftA2 (+) [3,2] [4]
[7,6]
Prelude Control.Applicative> liftA2 (+) [1,2] [10,100]
[11,101,12,102]
```

the result are given this way
[
    (1,10):11,
    (1,100):101,
    (2,10):12,
    (2,100):102,
]

#### Monad

We got to the famous graal of monads. It took me forever to get to understand what they are. Hopefully, you'll get there before me with this shortcut.

A monad `M` for a type `a` is an `enhanced` applicative which :

- has a `bind` function written `>>=`
    from (
      a monad instance `m1` of `M` having `a` type inside
      a function
        form
          `a` type
        to
          an instance `m2` of `M` having a type `b` inside
    )
    to an instance `m3` of `M` having a type `b` inside

- a `replacement` function written `>>`
    from
      a monad instance `m1` of `M` having `a` type inside
      a monad instance `m2` of `M` having `b` type inside
    to
      a monad instance `m3` of `M` having `b` type inside

The goal of monad is to chain operations allowing any to fail without having to define the failure case for each as in this classical pyramid of doom :


if success a
        if success b
              if success c
                exit success
              else
                exit failure c
        else
          exit failure b
    else
      exit failure c
else
  exit failure a


##### technical definition

```haskell
Prelude> :i Monad
class Applicative m => Monad (m :: * -> *) where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
  fail :: String -> m a
  {-# MINIMAL (>>=) #-}
  	-- Defined in ‘GHC.Base’
instance Monad (Either e) -- Defined in ‘Data.Either’
instance Monad [] -- Defined in ‘GHC.Base’
instance Monad Maybe -- Defined in ‘GHC.Base’
instance Monad IO -- Defined in ‘GHC.Base’
instance Monad ((->) r) -- Defined in ‘GHC.Base’
instance Monoid a => Monad ((,) a) -- Defined in ‘GHC.Base’
```

##### practical example

lets imagine we want a type that goes into error if we get out of a range [a,b]

This range will be our context.


```haskell

:{
type Range = (Int, Int)
type RangeNum = (Int, Range)


rangePlus :: Int -> RangeNum -> Maybe RangeNum
rangePlus x (y, (a, b))
  | x + y >= a && x + y <= b = Just (x + y, (a,b))
  | otherwise        = Nothing

:}

```

Now we can do operations without caring if one fails in the middle

```haskell
Prelude> return (1, (1,2)) >>= rangePlus 1
Just (2,(1,2))
Prelude> return (1, (1,2)) >>= rangePlus 1 >>= rangePlus 3
Nothing
```

We have an Int with a context and we can make operation as if the context was not present.


### All programs begin with the `IO monad`

There is a special monad that we need in haskell the `IO monad`.

Any program begin with it as it's the signature of main :

```haskell
main :: IO ()
```

#### Huston there is a problem

Haskell is about avoiding side effects ... but print to a terminal has side effects.
We don't want side effects but we want to print to the terminal.

<!-- illustrate the cake and the money of the cake -->

Here comes the `IO monad` it encapsulate the "`IO operations`" in a box.
We can put things in this box and haskell will manage it.

#### Interact with the shell

example : mix constant and user string.

Strings in haskell are immutable : each time I apply a function over a `String` I'll always get the same result.

```haskell
:t "hello"
"hello" :: [Char]
```

The only problem is that I may want to use a string written by the user. I can't know what it will be.
It's hell for `haskell`.

<!-- draw a priest with a cross in front of a guy writing to his keyboard -->

But well ... it's nice to able to speak with the user.


## Misc

An other example.

I want to write to a server only it responds.


```haskell
see network/write_once.hs
see network/write_loop.hs

```

Create a server allowing one client

```bash
nc -lk 3000
```


Create a server allowing multiple clients

```bash
socat - TCP-LISTEN:3000,fork,reuseaddr
```

Create a server with 2 distinct channels using file ch1 and ch2

```bash
socat -u tcp-l:3000,fork,reuseaddr system:'bash -c \"tee >(grep -oP \\\"(?<=#1).*?(?=#)\\\" > ch1) >(grep -oP \\\"(?<=#2).*?(?=#)\\\" > ch2) > /dev/null\"'
```

## Transport library

### Getting inspired

`barrelDB` has similarities with `couchDB` and there is an haskell implementation for [it](http://hackage.haskell.org/package/CouchDB-1.2.2/docs/Database-CouchDB.html)

The part concerning the transport layer is [here](hackage.haskell.org/package/CouchDB-1.2.2/docs/src/Database-CouchDB-HTTP.html).

It consists of a monad based encapsulating the IO monad and a connection.