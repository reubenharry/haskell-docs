Typeclasses add constraints to polymorphic types:

```haskell
notEqual :: Eq a => (a, a) -> Bool -- (1)!
notEqual (x,y) = not (x == y)
```

1. As usual, one can (with the `ExplicitForall` extension), write this: `forall a. Eq a => (a, a) -> Bool`, which you may find clearer.

The type of `notEqual` states: for any type `a` **such that `a` is an instance of the `Eq` typeclass**, give me a pair of `a`s and I will return a `Bool`.

!!! Note
    `Eq a` is not a type, but rather a different *kind* of entity, called a constraint.

    `Eq` is referred to as a typeclass.

## Typeclasses

Typeclasses, such as `Eq`, are defined as follows:

```hs
class  Eq a  where
    (==) :: a -> a -> Bool --(1)!
```

1. The actual definition has a second method, `(/=)`, omitted here for clarity.

Here, `(==)` is a method of the `Eq` typeclass.

To make a type be *an instance* of a type class, one writes a definition of the method(s) for the type in question:

```hs
instance Eq Bool where
    True == True = True
    False == False = True
    _ == _ = False
```


## Automatically deriving instances


```hs title="repl example"
> data Piece = Bishop | Knight deriving (Eq, Ord, Show)

> Bishop == Knight
False
> show Bishop
"Bishop"
> import Data.List
> sort [Knight, Bishop]
[Bishop,Knight]
```

## Typeclass error messages

```hs title="repl example"
> import Data.List

> data Piece = Bishop | Knight deriving Eq

> sort [Knight, Bishop]

"No instance for (Ord Piece) arising from a use of ‘sort’..."
```

The error is raised because `sort` has type `#!hs sort :: Ord a => [a] -> [a]`, which means that it expects as input, a list of values of a type which is an instance of the `Ord` class.


## Using typeclasses

### Constraints propagate

```hs
complexFunction :: Eq a => a -> ...
complexFunction x = let y = notEqual (x, x) in ...
```

Because `notEqual` is called on `(x, x)`, `x` must be of a type that is an instance of `Eq`. 

The compiler will reason in this way, even if you don't write a type signature.

## Inheritance

Semigroup a => Monoid a

todo 

more interesting: for any type `a`, if `a` has an `Eq` instance, then `[a]` also has an `Eq` instance.

Haskell is capable of making more complex deductions:
    todo

    Here,
        is an instance of `Eq` because 

    for example, if some type `X` is an instance of `Ord`, then 
    transitivity

!!! Tip
    A common difficulty that you may encounter is that you don't know what instance of a typeclass is being invoked:

    ```hs

    -- first example
    {-# LANGUAGE OverloadedStrings #-}
    import Data.Text
    example :: Text
    example = "hello" `append` mempty 
    

   

    In this case, you know the type of `mempty`, which is `mempty :: forall a. Monoid a => a`. However, you do not know which *instance* of `Monoid` is being used when `mempty` is called. Mousing over `mempty` in VSCode will reveal that the instance is `Text`.

    You can then look up `Text` on Hackage and find the [source](https://hackage.haskell.org/package/text-2.0.1/docs/src/Data.Text.html#line-351), which gives the definition of `mempty` for `Text`.




## Type classes over others *kinds*

