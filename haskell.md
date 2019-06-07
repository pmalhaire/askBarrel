# A road trip in Haskell : askBarrel a REPL client for barrelDB

- [Introduction](#introduction)
- [Let's enter the Haskell world](#lets-enter-the-haskell-world)
- [The smell of lisp](#the-smell-of-lisp)
- [Function are variables, variables are functions](#function-are-variables-variables-are-functions)
  - [`:type`](#type)
  - [`:info`.](#info)
- [Meet the haskell syntax](#meet-the-haskell-syntax)
  - [The case of `$`](#the-case-of-)
- [The monad universe](#the-monad-universe)
  - [A big picture](#a-big-picture)
  - [functor](#functor)
    - [technical definition](#technical-definition)
    - [practical example](#practical-example)
  - [applicative](#applicative)
    - [technical definition](#technical-definition-1)
    - [practical example](#practical-example-1)
  - [Monad](#monad)
    - [technical definition](#technical-definition-2)
  - [practical example](#practical-example-2)
  - [All programs begin with the `IO monad`](#all-programs-begin-with-the-io-monad)
    - [Huston there is a problem](#huston-there-is-a-problem)
    - [Interact with the shell](#interact-with-the-shell)
- [conclusion](#conclusion)


## Introduction

Now that the high level design is clear enough to be used. Let's introduce `haskell`. This project is my first in this wonderful language and I took this as an opportunity to use my fresh eyes on the language to introduce it.

## Let's enter the Haskell world

Let's take a breath and introduce `Haskell`. Sometime when I go to conferences I fell like `Haskell` is a label for programmers, if you know `Haskell` you must be smart. Well ... maybe that's why you are reading my post. Smart or not `Haskell` is `nerd` fun.

When you learn new languages it's practical to use analogies from one language to an other. A lot among us learned `Html` then `Javascript` then `PHP` then `Cpp` or in other combination including `python`, `lua`, `golang`, `ruby` or many other languages.
Learning `Haskell` is not a jump from the previous language you learned (unless you've studied an other functional language). You jump to a new world. You go back to the "noob" state and learn something new at each new `Haskell` line. This is refreshing and offers your brain a brain new vision of computer science.

Introducing `Haskell` is not that easy since as said it's a jump to a new world. I'll take a rather unusual way, if you want "classical" ways you'll see interesting books in the references.

## The smell of lisp

Haskell reminds me the time I studied lisp in the IA course of professor [Tupia](https://www.pucp.edu.pe/profesor/manuel-tupia-anticona) in the Pontificia Universidad Catolica del Péru.

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

## Function are variables, variables are functions

One of the first thing we need to make computation are variables and functions. Forget what you know about imperative programing `Haskell` does not work that way let's play around arithmetic operations to feel it.

### `:type`

Let's use `:type` in `ghci` after all `Haskell` is all about type.

```haskell
ghci
λ> :type 1
1 :: Num p => p
```

```haskell
λ> :type +

<interactive>:1:1: error: parse error on input ‘+’
```

Oops, it seems that `+` no type. In fact is does have one but `+` is a special character to get it's type we need to use parenthesis.

```haskell
:t (+)
(+) :: Num a => a -> a -> a
```

Indeed we got back to the num class where `+` is defined.
This strange but beautiful `a -> a -> a` explains that `+`
is a function of 2 variables of type `a` returning the type `a`. You may wonder why we have several `->` it's because any function in `haskell` can be seen construction maid of function of one variable, it may seem strange at first but it's one of the things that make `haskell` powerful.

Let's take an example

```haskell
λ> :t 1 + 2
1 + 2 :: Num a => a
λ> :t (1 +)
(1 +) :: Num a => a -> a
λ> :t (+ 2)
(+ 2) :: Num a => a -> a
```

So `(1 +)` is a function of one variable returning one variable let's use it.

once

```haskell
λ> (1 +) 2
3
```

several times

```haskell
λ> (1 +) (1 +) 2

<interactive>:61:1: error:
    • Non type-variable argument in the constraint: Num (a -> a)
      (Use FlexibleContexts to permit this)
    • When checking the inferred type
        it :: forall a. (Num a, Num (a -> a)) => a
```

Here `haskell` sees 3 block so it try to apply `(+1)` using the 2 following arguments `(1 +)` and 2
We need to help him with parenthesis here since `(1 +)` is a function one variable.

```
λ> (1 +) ((1 +) 2)
4
```

This kind of manipulation will be very useful as it allows to chain multiple function doing the calculation only when the result is needed. It's called [`Currying`](https://wiki.haskell.org/Currying) and gave it's name to `Haskell` since [`Haskell Curry`](https://en.wikipedia.org/wiki/Haskell_Curry) invented this.

Note you can use only `:t` as shortcut for `:type`. I use type to emphasis the fact we are asking the `type` here

### `:info`.

An other very useful function in ghci is `info` it gives all information about a type, a variable or a function.

```haskell
λ> :t 1
1 :: Num p => p

λ> :info Num
class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
  {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
  	-- Defined in ‘GHC.Num’
instance Num Word -- Defined in ‘GHC.Num’
instance Num Integer -- Defined in ‘GHC.Num’
instance Num Int -- Defined in ‘GHC.Num’
instance Num Float -- Defined in ‘GHC.Float’
instance Num Double -- Defined in ‘GHC.Float’
λ>
```

So `1` is of type `Num`. Let's get to know Num using `:info`.

`Num` is a `class` and has 7 functions defined with it let's focus on +.

```haskell
λ> :info +
class Num a where
  (+) :: a -> a -> a
  ...
  	-- Defined in ‘GHC.Num’
infixl 6 +
```
Here info gives us addition info about `+`, it is a function defined for `Num` and is `infixl` with a precedence of `6`.

`infixl` means that I can use `+` with the first argument at it's left.

The precedence of `6` means that if other `infix` operator are present it will be applied after those of precedence `7` to `10` (`10` is the max). `*` is of higher precedence :

```haskell
λ> :i *
class Num a where
  ...
  (*) :: a -> a -> a
  ...
  	-- Defined in ‘GHC.Num’
infixl 7 *
```

## Meet the haskell syntax

Haskell is a rich universe with many entrance doors.

Mine is somehow singular, I find `Haskell` syntax graphically beautiful.

### The case of `$`

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


Let's write it lisp's way.

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

`$` twice :

```bash
λ> (/) 1000 $ (/) 100  $ (/) 5  2
25.0
```

It seams like a toy, but it's very useful.


For more details see [`infix functions`](https://wuciawe.github.io/functional%20programming/haskell/2016/07/03/infix-functions-in-haskell.html).

## The monad universe

Monad has became a mythical creature in programming concepts universe. I used it to look smart when I did not understand it at all. (I wonder if I lied to others or to myself when I said I knew what monad were)

I won't lie, it's not a pain free to learn it. And to be honest I am not entirely comfortable with it. But it's conceptually interesting and helped me to approach many concepts I didn't even note before.

I'll expose one by one the concepts behind monads. Your brain will slowly be surely assemble the puzzle. You'll fell fireworks in your brain. Don't panic it's normal.

### A big picture

<!-- draw inclusion of concepts -->
Illustration:

(Monad (applicative (functor)))


### functor

A functor is a box with which :
- contains zero or more instances `i1, ... in` of a type `a`
- can apply a function from type `a` to type `b` on each of `i1, ... in` resulting on `u1, ... un`.


#### technical definition

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

#### practical example

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

### applicative

An applicative `A` is an `enhanced` functor which :
- has a function from `a` to `A` ( kind of from `a` to `boxed` `a` )
- has a function
    from :
        1 : functions from type `a` to type `b` `boxed` in the instance A1 of `A`
        2 : values of type `a` `boxed` in the instance A2 of `A`
    to :
        values of type `b` `boxed` in the instance A3 of `A`

#### technical definition

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

#### practical example

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

### Monad

We got to the famous graal of monads. It took me forever to get to understand what they are. Hopefully, you'll get there before me with this shortcut.

A monad is like a medicine pack. It's a box with constrains on how to use it's content.

<!-- Problems with your code ? use monad ! Twice a day it make your code flow
 ___________
|\   \      |
| \ M \     |
|  \ O \    |
|   \ N \   |
|    \ A \  |
|     \ D \ |
|      \   \|
|___________|
-->

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


#### technical definition

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

### practical example

lets imagine we want a type that goes into error if we get out of a range [a,b]

This range will be our context.


```haskell
-- use :{ and :} to use this code in ghci
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

## conclusion

I tried here to give you an introduction on what is haskell and some of it's useful tools we'll need to build our `barrelDB` client.
