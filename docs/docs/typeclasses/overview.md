---
comments: true
---

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

## Constraint implication (classes)

One typeclass may depend on another:

```hs hl_lines="4"
class Semigroup a where
    (<>) :: a -> a -> a

class Semigroup a => Monoid a where
    mempty :: a
```

What this means is that any instance of [Monoid](/typeclasses/survey/#monoid) must first be an instance of [Semigroup](/typeclasses/survey/#semigroup) (as well as implementing the `Monoid` method `mempty`).

This means that **if** you encounter a type that is an instance of `Monoid`, **then** it will be an instance of `Semigroup` and so you can use the method `<>`. For this reason, this is often called *inheritance* , although the relationship to inheritance in other languages is not direct.

## Constraint implication (instances)

```hs
instance Eq a => Eq [a] where
    ls1 == ls2 = ...
```

Read this as saying: for *any* type `a`, **if** `a` is an instance of `Eq`, **then** `[a]` is also an instance of `Eq`. 

!!! Warning
    Haskell can be a little picky about when you are allowed to do this, but bear in mind that mostly you will be *using* typeclasses and instances, rather than writing your own.

This allows Haskell's type checker to make potentially quite complex deductions. For example:

```hs
compareLists :: Ord a => ([a], [a]) -> Bool
compareLists (x, y) = x == y
```

Haskell knows that `==` can be called on `x` and `y`. How?

1. It knows that `x` and `y` both have type `[a]`
2. It knows that `a` is an instance of `Ord` (from the type signature)
3. It knows that `Ord a` implies `Eq a`
4. It knows that `Eq a` implies `Eq [a]`





!!! Note

    Libraries like [lens](https://hackage.haskell.org/package/lens) use the ability of the type checker to make these deductions in sophisticated ways.



## Typeclass error messages

```hs title="repl example"
> import Data.List

> data Piece = Bishop | Knight deriving Eq

> sort [Knight, Bishop]

"No instance for (Ord Piece) arising from a use of ‘sort’..."
```

The error is raised because `sort` has type `#!hs sort :: Ord a => [a] -> [a]`, which means that it expects as input, a list of values of a type which is an instance of the `Ord` class.


## Using typeclasses

!!! Warning 

    It is recommended that you avoid creating your own type classes unless it is entirely necessary. This is because:

    1. There is usually a solution to a problem which doesn't require typeclasses.
    2. It is easy to create a typeclass that is badly designed.

    Instead, rely on existing type classes from libraries. 


### Constraints propagate

```hs
complexFunction :: Eq a => a -> ...
complexFunction x = let y = notEqual (x, x) in ...
```

Because `notEqual` is called on `(x, x)`, `x` must be of a type that is an instance of `Eq`. 

The compiler will reason in this way, even if you don't write a type signature.



!!! Tip
    A common difficulty that you may encounter is that you don't know what instance of a typeclass is being invoked:

    ```hs

    -- first example
    {-# LANGUAGE OverloadedStrings #-}
    import Data.Text
    example :: Text
    example = "hello" `append` mempty 
    ```

   

    In this case, you know the type of `mempty`, which is `mempty :: forall a. Monoid a => a`. However, you do not know which *instance* of `Monoid` is being used when `mempty` is called. Mousing over `mempty` in VSCode will reveal that the instance is `Text`.

    You can then look up `Text` on Hackage and find the [source](https://hackage.haskell.org/package/text-2.0.1/docs/src/Data.Text.html#line-351), which gives the definition of `mempty` for `Text`.



## Type class recursion

Type class instances may use the very method they are defining in the definition.

```hs
instance Eq (Int, Int) where
    (x, y) == (x', y') = (x == x') && (y == y')
```

!!! Note

    Here, `==` on the right hand side of the definition is the `Eq` method for `Int`, but on the right hand side, it is the method for `(Int, Int)`.

```hs title="repl example"
> (4,3) == (4,3)
True
```

## Type classes over others *kinds*

