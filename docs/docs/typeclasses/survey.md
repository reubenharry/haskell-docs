---
comments: true
---

# Under :construction:

The best way to understand any given typeclass is to find its documentation online, inspect its methods, and look at some instances. This is usually easy with Google, or failing that, Hoogle.

Here is an example, the `Semigroup` class:

![](/img/semigroup)

To understand what methods the class requires for its instances, see "minimal complete definition" where `(<>)` is the only (required) method. See below for its type.

!!! Note

    You will also see a comment about associativity, which is a property that instances should have. This property can't be automatically enforced, so is the responsibility of the writer of the instance.

The next step is to inspect some instances, which are also listed below, like:

![](/img/semigrouplist)


## [Show](https://hackage.haskell.org/package/base-4.17.0.0/docs/GHC-Show.html#t:Show)

A class for converting a type into a `String`, which can be displayed. `String` is a [legacy](/gotchas/strings) type, but `Show` is widely used, and can be derived by Haskell:

```hs title="repl example"
> data Piece = Bishop | Knight deriving Show
> show Bishop
"Bishop"
```

Most instances that should exist do exist. For example:

- `#!hs instance Show Int`
- `#!hs instance Show Bool`
- `#!hs instance Show a => Show [a]`
- `#!hs instance Show a => Show (Maybe a)`

!!! Gotcha

    ```hs title="repl example"
    > show (+)
    "No instance for (Show (Integer -> Integer -> Integer))..."
    ```

    Haskell won't show arbitrary functions, because they don't have a `Show` instance. This makes sense, since it is impossible to show all the (infinite) input-output pairs of a function.

## [Eq](https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-Eq.html#t:Eq)

## Ord

## Num 

## [Semigroup](https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-Semigroup.html)

Provides a method to combine two values: `#!hs (<>) :: a -> a -> a`

Any instance should define `<>` such that it is associative (i.e. `a <> (b <> c) = (a <> b) <> c`)

### `instance Semigroup Text`


```hs title="repl example"
> :set -XOverloadedStrings -- (1)!
> import Data.Text
> text = "hello"
> text2 = "world"
> text <> text2
"helloworld"
```

1. See [here](/gotchas/strings) for explanation of why this is needed.




## Monoid

??? Info
    For historical reasons (`Monoid` predates `Semigroup`), `Monoid` has a method `mappend` which is redundant given the inheritance of `<>` from `Semigroup`.

Under :construction:

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

### `instance Functor Maybe`

```hs title="repl example"
> maybeChar = Just 'a'
> :t maybeChar
maybeChar :: Maybe Char
> fmap (=='a') maybeChar
Just True

> :t fmap (=='a') maybeChar
fmap (=='a') maybeChar :: Maybe Bool

> fmap (=='a') Nothing
Nothing
```

### `instance Functor (Either a)`

!!! Note
    `Either a` is `Either` [partially applied]() to `a`, and has [kind]() `* -> *` as required.

    Under :construction:


```hs title="repl example"
> eitherChar = Right 'a'
> :t eitherChar
eitherChar :: Either a Char -- (1)!
> fmap (=='a') eitherChar
Right True

> :t fmap (=='a') eitherChar
fmap (=='a') eitherChar :: Either a Bool

> other = Left True
> :t other
other :: Either Bool b
> fmap (=='a') other
Left True
> :t fmap (=='a') other
fmap (=='a') other :: Either Bool Bool -- (2)!
```

1. Haskell correctly infers that `a` can be [any](/basics/types/#universal-types) type.

2. Haskell correctly infers the type, which is no longer universally quantified.

### Reader r

### State s

### Fix f

### Free f

### Cont r

## Applicative

## Monad

