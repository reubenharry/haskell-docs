Common typeclasses include: `Show`, `Eq`, `Ord`, `Num`, `Monoid`, `Foldable`, `Functor` and `Monad`

todo: explain each one

## Show

## Eq

## Ord

## Num 

## [Semigroup](https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-Semigroup.html)

Provides a method to combine two values: `#!hs (<>) :: a -> a -> a`

Any instance should define `<>` such that it is associative (i.e. `a <> (b <> c) = (a <> b) <> c`)

### Text

```hs title="repl example"
> :set -XOverloadedStrings
> import Data.Text
> text = "hello"
> text2 = "world"
> text <> text2
"helloworld"
```




## Monoid

### Any

### All

### Sum 

### Product


## Foldable

## [Functor](https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-Functor.html#t:Functor)

```hs
class Functor (f :: * -> *) where -- (1)!
    fmap :: (a -> b) -> f a -> f b
```

1. The *kind signature* `f :: * -> *` requires the [GHC2021](/gettingstarted/versions/#extensions) standard extensions.


!!! Hint
    Types which are instances of `Functor` must have kind `* -> *`.

    So `Int` or `Bool` or `Either Int Bool` or `[Int]` **cannot be instances of `Functor`, but `Either Int`, or `[]` can. (See section on [partial application of types](/basics/functions/#partial-application-for-types).)


### List

The definition of `fmap` for `[]` is just [map](/thinkingfunctionally/hof/#map)

```hs title="repl example"
> ls = [1 :: Int, 2, 3]
> :t ls 
ls :: [Int]
> fmap even ls
[False,True,False]
> :t fmap even ls
fmap even ls :: [Bool]
```

### Maybe

```hs title="repl example"
> maybeChar = Just 'a'
> :t maybeChar
maybeChar :: Maybe Char
> fmap (=='a') maybeChar
Just True
> :t fmap (=='a') maybeChar
fmap (=='a') maybeChar :: Maybe Bool
```

### Reader r

### State s

### Fix f

### Free f

### Cont r

## Applicative

## Monad

