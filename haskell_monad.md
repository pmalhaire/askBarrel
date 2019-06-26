# An Haskell trip [part 3] : askBarrel a REPL client for barrelDB

- [The monad universe](#The-monad-universe)
  - [A big picture](#A-big-picture)
  - [functor](#functor)
    - [technical definition](#technical-definition)
    - [practical example](#practical-example)
  - [applicative](#applicative)
    - [technical definition](#technical-definition-1)
    - [practical example](#practical-example-1)
  - [Monad](#Monad)
    - [technical definition](#technical-definition-2)
  - [practical example](#practical-example-2)
  - [All programs begin with the `IO monad`](#All-programs-begin-with-the-IO-monad)
    - [Huston there is a problem](#Huston-there-is-a-problem)
    - [Interact with the shell](#Interact-with-the-shell)
- [conclusion](#conclusion)

## The monad universe

Monad has became a mythical creature in programming concepts universe. I used it to look smart when I did not understand it at all. (I wonder if I lied to others or to myself when I said I knew what monad were)

I won't lie, it's not a pain free to learn it. And to be honest I am not entirely comfortable with it. But it's conceptually interesting and helped me to approach many concepts I didn't even note before.

I'll expose one by one the concepts behind monads. Your brain will slowly but surely assemble the puzzle. You'll fell fireworks in your brain. Don't panic it's normal.

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
