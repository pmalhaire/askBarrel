# Discovering Haskell while creating a repl DB client for barrelDB


<!-- @import "[TOC]" {cmd="toc" depthFrom=1 depthTo=4 orderedList=false} -->

<!-- code_chunk_output -->

* [Discovering Haskell while creating a repl DB client for barrelDB](#discovering-haskell-while-creating-a-repl-db-client-for-barreldb)
	* [introduction](#introduction)
	* [Where do I begin ?](#where-do-i-begin)
	* [Transport library](#transport-library)
		* [Getting inspired](#getting-inspired)
	* [Haskell is `nerd` fun](#haskell-is-nerd-fun)
		* [The smell of lisp](#the-smell-of-lisp)
		* [Equivalents things are beautifull](#equivalents-things-are-beautifull)
			* [The case of `$`](#the-case-of)
		* [The monad world](#the-monad-world)
			* [functor](#functor)
			* [applicative](#applicative)
			* [monoid](#monoid)
			* [Monad](#monad)

<!-- /code_chunk_output -->

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

#### The case of `$`

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


For more details see [`infix functions`](https://wuciawe.github.io/functional%20programming/haskell/2016/07/03/infix-functions-in-haskell.html).

### The monad world

Monad has became with years a mythical creature of programming concepts. I won't lie it's not a pain free to learn... and to say the truth I did had the chance to demonstrate how useful it is. It's definetely conceptual interresting as it mixes many concepts most programmers manupulate whithout understanding them.

I won't explain it as but I'll expose the concepts monads contains to let your brain picture it like a puzzle.

Illustration:

(Monad (monoid (applicative (functor))))


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
        1 : fonctions functions from type `a` to type `b` `boxed` in the instance A1 of `A`
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

example : Maybe is a an applicative it's used to have a `simple` box which is empty or contain a value. A list of one or zero value

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


An applicative usage compare without `unboxing`

```haskell
Prelude> (==) <$> (Just 1) <*> (Just 2)
Just False
```

An applicative usage test values without `unboxing`

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


#### monoid

A monoid `m` is composed :

- `f` an associative binary (two parameters) function
    associative means ( x `f` y ) `f` z = x `f` ( y `f` z) =  x `f` y `f` z
    binary means the function take two parameters

- `e` an element that act as identity for `f`
    identity means `f` `e` = `e`

- an `unlist` function which take a list of instances of `m` and returns an instance of `m`


##### technical definition

```haskell
Prelude> :i Monoid
class Semigroup a => Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a
  {-# MINIMAL mempty #-}
  	-- Defined in ‘GHC.Base’
instance Monoid [a] -- Defined in ‘GHC.Base’
instance Monoid Ordering -- Defined in ‘GHC.Base’
instance Semigroup a => Monoid (Maybe a) -- Defined in ‘GHC.Base’
instance Monoid a => Monoid (IO a) -- Defined in ‘GHC.Base’
instance Monoid b => Monoid (a -> b) -- Defined in ‘GHC.Base’
instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e) =>
         Monoid (a, b, c, d, e)
  -- Defined in ‘GHC.Base’
instance (Monoid a, Monoid b, Monoid c, Monoid d) =>
         Monoid (a, b, c, d)
  -- Defined in ‘GHC.Base’
instance (Monoid a, Monoid b, Monoid c) => Monoid (a, b, c)
  -- Defined in ‘GHC.Base’
instance (Monoid a, Monoid b) => Monoid (a, b)
  -- Defined in ‘GHC.Base’
instance Monoid () -- Defined in ‘GHC.Base’
```

##### practical example

string is a monoid

`++` is it's associative binary function

```haskell
Prelude> "a" ++ ( "b"  ++ "c" )
"abc"
Prelude> ( "a" ++ "b" )  ++ "c"
"abc"
```


`++` "" is identity

```haskell
Prelude> "a" ++ ""
"a"
```

mconcat is the `unlist` function


```haskell
Prelude> mconcat  ["aa","b"]
"aab"
```

#### Monad

We got to the famous grall of monads. It took me forever to get to understand what they are. Hopefully, you'll get there before me with this shortcut.

A monad `M` for a type `a` is an `enhanced` applicative which :

- has a `bind` function writen `>>=`
    from (
      a monad instance `m1` of `M` having `a` type inside
      a function
        form
          `a` type
        to
          an instance `m2` of `M` having a type `b` inside
    )
    to an instance `m3` of `M` having a type `b` inside

- a `replacement` function writem `>>`
    from
      a monad instance `m1` of `M` having `a` type inside
      a monad instance `m2` of `M` having `b` type inside
    to
      a monad instance `m3` of `M` having `b` type inside

The goal of monad is to chain operations allowing any to fail whithout having to define the failure case for each as in this classical pyramid of doom :


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

An other example.

I want to write informations to a server only if the server responds.


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

