# An Haskell trip [part 2] : let's discover haskell syntax

- [Introduction](#introduction)
- [Let's enter the Haskell world](#lets-enter-the-haskell-world)
- [Meet the haskell syntax](#meet-the-haskell-syntax)
  - [Meet `::` using `:type`](#meet--using-type)
  - [Meet `->` (arrow) using `:type`](#meet---arrow-using-type)
  - [Meet the `=>` double arrow using `:info`](#meet-the--double-arrow-using-info)
  - [The case of `$`](#the-case-of-)
- [[Interlude] The smell of lisp](#interlude-the-smell-of-lisp)
- [Conclusion](#conclusion)
- [Appendix : references](#appendix--references)


## Introduction

Now that the high level design is clear enough to be used. Let's introduce `haskell`. This project is my first in this wonderful language and I took this as an opportunity to use my fresh eyes on the language to introduce it.

## Let's enter the Haskell world

Let's take a breath and introduce `Haskell`. Sometime when I go to conferences I fell like `Haskell` is a label for programmers, if you know `Haskell` you must be smart. Well ... maybe that's why you are reading my post. Smart or not `Haskell` is `nerd` fun.

When you learn new languages it's practical to use analogies from one language to an other. A lot among us learned `Html` then `Javascript` then `PHP` then `Cpp` or other order/combination including `python`, `lua`, `golang`, `ruby` or many other languages.
Learning `Haskell` is not a jump from the previous language you learned (unless you've studied an other functional language). It's a new world each new `Haskell` line will learn you something. This offers your brain a brand new vision of computer science.

Introducing `Haskell` is not that easy since as said it's a jump to a new world. I'll take a rather unusual way, if you want "classical" ways you'll see interesting books in the references. The goal here is to introduce the concepts we need to create askBarrel.

## Meet the haskell syntax

Haskell is a rich universe with many entrance doors.

Mine is somehow singular, I find `Haskell` syntax graphically beautiful.

One thing that can loose you when learning `haskell` is the symbols such as `=>`, `::`, `->`, `$`,`<$`. It's based on many useful concepts. Once the concept is yours the symbol will come naturally. You'll ask yourself I want to do that I know there is a glyph for it.

The goal of this project is to make a `REPL` parser. Our main objects are `strings` so we'll focus on example concerning strings.

A useful [link](https://github.com/takenobu-hs/haskell-symbol-search-cheatsheet) to help you google about symbols.

### Meet `::` using `:type`

Let's write a string `test` in ghci.

```haskell
ghci
λ> "test"
"test"
```

Let's use `:type` (or it short form `:t`) in `ghci` after all `Haskell` is all about type.

```haskell
ghci
λ> :t "test"
"test" :: [Char]
```

We meet here the `::` symbol. Double colon or type signature.
It tells the compiler that our `string` is of type `[Char]` a list of `Char`.

`[Char]` is the default type used for string but we can use `::` to force an other type `String` for example.

```haskell
ghci
λ> "test"::String
"test"
λ> :t "test"::String
"test"::String :: String
```

### Meet `->` (arrow) using `:type`

One `String` is good several is more useful.

Let's concatenate 2 strings.

```haskell
ghci
λ> "first" ++ "second"
"firstsecond"
```

Let's get the type again.

```haskell
ghci
λ> :t ++
<interactive>:1:1: error: parse error on input ‘++’
```

Oops, it seems that `++` no type. In fact is does have one but `++` is composed of special character `+` to get it's type we need to use parenthesis.

```haskell
ghci
λ> :t (++)
(++) :: [a] -> [a] -> [a]
λ>
```

This strange but beautiful `[a] -> [a] -> [a]` explains that `++` is a function of 2 variables of type `[a]` returning the type `[a]`. You may wonder why we have several `->` it's because any function in `haskell` can be seen as a construction made of functions of one variable, it may seem strange at first but it's one of the things that make `haskell` powerful.

Let's break `++` into parts adding parenthesis.

```haskell
ghci
λ> ("first" ++) "second"
"firstsecond"
λ> :t ("first" ++)
("first" ++) :: [Char] -> [Char]
```

So `("first" ++)` type signature (right of `::`) is `[Char] -> [Char]`.

From an argument of type `[Char]` return (`->`) one value of type `[Char]`.

Let's go back on `(++)`.

```haskell
ghci
λ> :t (++)
(++) :: [a] -> [a] -> [a]
λ>
```

`(++)` type signature `[a] -> [a] -> [a]` tells us that :

- if I give one argument to `++` such as in `("first" ++)` it will return a function taking `[a]` and returning `[a]`.
- if I give 2 arguments to `++` such as in `"first" ++ "second"` it will return a instance of type `[a]`.


This kind of manipulation will be very useful as it allows to chain multiple function doing the calculation only when the result is needed. It's called [`Currying`](https://wiki.haskell.org/Currying) and gave it's name to `Haskell` since [`Haskell Curry`](https://en.wikipedia.org/wiki/Haskell_Curry) invented this.

It works as expected. `Emina` my noble colleague made me realize that it was not that instinctive. Meanwhile we are looking at one of `Haskell`'s beauty. A way to see it is to cut type `signature` as follow.

```haskell
-- to use the function
| parameters          | result |
   [a] -> ... -> [a]  ->    [a]

-- to use partially the function
| parameters          |      result |
   [a] -> ... -> [a]  ->  [a] -> [a]
-- here we will have a function you'll need one more `[a]` to get a result such as ("first" ++)
```

### Meet the `=>` double arrow using `:info`

An other very useful function in ghci is `info` it gives all information about a type, a variable or a function.

```haskell
λ> :t 1
1 :: Num p => p
```

Here our type signature (`::`) as an extra `=>`.
This add a constraint to the type `p`. `p` must be any type that is `Num` instance.

Let's get more about it using `:info`.

```haskell
ghci
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

To be valid the type p must implement the 7 functions defined in the class `Num`.

Note : the `class` here is not the one of object oriented programing.

In our `ghci` context the available instances of Num are

```haskell
instance Num Word -- Defined in ‘GHC.Num’
instance Num Integer -- Defined in ‘GHC.Num’
instance Num Int -- Defined in ‘GHC.Num’
instance Num Float -- Defined in ‘GHC.Float’
instance Num Double -- Defined in ‘GHC.Float’
```

We could implement more if we wanted to. It's an other beauty of `haskell` we tell a contract for a variable `a` here `Num` and any instance matching the contract can be used `Word`, `Integer`, etc.

```haskell
ghci
λ> :t 1.1
1.1 :: Fractional p => p
λ> :t 1
1 :: Num p => p
λ> 1.1 +1
2.1
```

### The case of `$`

Consider the following we want to add a head and a tail to our string.

```haskell
ghci
λ> "head-" ++ "content" ++ "-tail"
"head-content-tail"
```

It will be more usefull to do something like `headtail "content"`.

Let's reuse what we have done before.

`"head-"` is to be added first so we are fine

```haskell
ghci
λ> "head-" ++ "content"
```

`"-tail"` in must be added after.
We will create a function that adds `"-tail"` after.

```haskell
ghci
λ> (++ "-tail") "content"
"content-tail"
```

Now we can do `headtail` like

```haskell
ghci
λ> (++ "-tail") ( "head-" ++ "content" )
"head-content-tail"
```


Here parenthesis are clearly needed if we omit then we got

```haskell
ghci
λ> (++ "-tail") "head-" ++ "content"
"head--tailcontent"
```

We can even do better using a partial function for head as well.

```haskell
ghci
λ> ("head-" ++) ((++ "-tail") "content")
"head-content-tail"
```

It's good but it's a lot of parenthesis.

Let me introduce `$`. It's an operator to use less parenthesis, it wraps what is after it with parenthesis.

```haskell
ghci
λ> ("head-" ++) $ (++ "-tail") "content"
"head-content-tail"
```

Let's create a function

```haskell
ghci
λ> headtail a = ("head-" ++) $ (++ "-tail") a
λ> headtail "content"
"head-content-tail"
```

## [Interlude] The smell of lisp

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

I did not focus on the fact that `++` is an `infix function`. To get to understand it try to move `++` one side or the other `(++ "string")` and `("string" ++)`.

For more details see [infix functions](https://wuciawe.github.io/functional%20programming/haskell/2016/07/03/infix-functions-in-haskell.html).

## Conclusion

We meet the `Haskell` syntax with the goal manipulate strings our basic tool for a `REPL` client. In the next part we'll tackle the `M word` the mythical creature called `Monad`.

## Appendix : references

- [Learn You a Haskell](http://learnyouahaskell.com/)
- [Haskell symbol search cheat sheet](https://github.com/takenobu-hs/haskell-symbol-search-cheatsheet)
- [infix functions](https://wuciawe.github.io/functional%20programming/haskell/2016/07/03/infix-functions-in-haskell.html).