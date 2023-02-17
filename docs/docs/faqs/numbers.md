---
comments: true
---

What does the type of number literals mean, e.g. `#!hs 5 :: Num a => a` (or more verbosely: `#!hs 5 :: forall a. Num a => a`)?

Read `n :: Num a => a` as saying: `n` has any type whatsoever, *as long as that type is an instance of the `Num` typeclass*.

Haskell has a [typeclass](/typeclasses/overview) `Num` for [numbers](/typeclasses/survey/#num) which generalized across various concrete number types like `Double`, `Float`, `Int` and `Integer`.

Haskell will always give the most general typeclass that supports the numeric operations you are using:

```hs title="repl example"
> :t 5 + 3
5 + 3 :: Num a => a -- (1)!

> :t 5 / 3
5 / 3 :: Fractional a => a -- (2)!

> :t 5 ** 3
5 ** 3 :: Floating a => a -- (3)!

```

1. `(+)` is an operation supported by the `Num` typeclass.
2. `(/)` is an operation supported by `Fractional`, a class that inherits from `Num`
3. `(**)` is an operation supported by `Floating`, a class that inherits from `Num`


[Like all universally quantified values](/basics/types/#how-to-use), a value of type `#!hs forall a. Num a => a` can be given as input to *any function that takes an concrete number type*:

```hs title="repl example"
> n = 5
> :t n
n :: Num a => a
> double = (\x -> x + x) :: Double -> Double
> double n
10
```