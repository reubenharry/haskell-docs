---
comments: true
---

Typeclasses allow the user to add *constraints* to [universally quantified types](/basics/types/#universal-types):

=== "without explicit quantifier (standard)"

    ```haskell
    notEqual :: Eq a => (a, a) -> Bool 
    notEqual (x,y) = not (x == y)
    ```

=== "with explicit quantifier"

    ```haskell
    notEqual :: forall a. Eq a => (a, a) -> Bool -- (1)!
    notEqual (x,y) = not (x == y)
    ```

    1. Requires the `ExplicitForall` extension.

The type of `notEqual` states: for any type `a` **such that `a` is an instance of the `Eq` typeclass**, given a pair of `a`s, this will return a `Bool`.

!!! Note
    `Eq a` is not a type, but rather a different *kind* of entity, called a **constraint**.

    `Eq` is referred to as a **typeclass**.

## Basics

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

## Different instances for the same type

A single type can have at most one instance for a typeclass.

However, two types which are [isomorphic](/basics/createData/#isomorphic-types) can have different instances:

```hs
import qualified Data.Text as T

data InterspersedText = MkI {getText :: T.Text} deriving Show -- (1)!

instance Semigroup InterspersedText where  --(2)!
    (MkI t1) <> (MkI t2) = MkI 
        $ T.concat 
        $ fmap (\(c1, c2) -> T.pack [c1,c2]) 
        $ T.zip t1 t2
```

1. `InterspersedText` and `Text` contain the same data in the sense that `MkI :: Text -> InterspersedText` and `getText :: InterspersedText -> Text` map back and forth [losslessly](/basics/createData/#isomorphic-types).

2. This isn't a sensible instance in practice and is only exemplary, not least because it breaks [the associativity law](/typeclasses/survey/#semigroup) for `Semigroup`

`InterspersedText` and `Text` contain the same information, but have different `Semigroup` instances. 

```hs title="repl example"
> (MkI "foo") <> (MkI "bar")
MkI "fboaor"

> "foo" <> "bar"
"foobar"
```

For a practical example, see the [Sum](/typeclasses/survey/#sum) and [Product](/typeclasses/survey/#product) types. The type `Sum Int` is [isomorphic](/basics/createData/#isomorphic-types) to `Int`, but they have different `Monoid` instances.

```hs title="repl example"

-- the (Sum Int) Monoid
> Sum 4 -- (1)!
Sum {getSum = 4}
> :t Sum 4
Sum 4 :: Num a => Sum a
> Sum 4 <> Sum 5
Sum {getSum = 9}

-- the (Product Int) Monoid
> Product 4
Product {getProduct = 4}
> :t Product 4
Product 4 :: Num a => Product a
> Product 4 <> Product 5
Product {getProduct = 20}
```

1. `Sum` is both the function which maps a number to a value of the `Sum` type, and the name of the type. See this page on [punning](/gotchas/punning/).

??? Note
    One other useful example is the logarithm type `Log`, from the `log-domain` package.

    ```hs
    > import Numeric.Log
    > :i Log -- (1)!
    newtype Log a = Exp {ln :: a} -- (2)!
    ...
    instance RealFloat a => Num (Log a) --(3)!

    > Exp 0 
    1.0 -- (4)!
    > Exp 0 * Exp 0 -- (5)!
    2.0

    ```

    1. `:i` prompts the repl to give info about an expression.
    2. The definition of the `Log` type.
    3. Read this as saying: "if the type `a` is an instance of `RealFloat`, then the type `Log a` is an instance of `Num`. See [this section](/typeclasses/overview/#constraint-implication-instances) for more.
    4. `Exp 0` represents the real number `1`, but under the hood, stores it in log-space as `0`.
    5. The definition of `*` (which is part of the instance for `Num`) adds the log-space numbers, rather than multiplying the real-space numbers. 


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

Similarly:

```hs
instance Num a => Monoid (Sum a)
```

This states that if `a` is an instance of `Num` (as are e.g. `Int` and `Double`) then `Sum a` (or concretely, `Sum Int` or `Sum Int`) are instances of `Monoid`.

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


### Constraints float up

```hs title="repl example"
5 :: Num a => a

> :t 5
5 :: Num a => a
> :t [5, 4]
[5, 4] :: Num a => [a] -- (1)!

> :t (True, 4)
(True, 4) :: Num b => (Bool, b)

-- with custom type
> data Square a = Empty | Piece a
> :t Piece 4
Piece 4 :: Num a => Square a
```

1. **Not** `#!hs [forall a . Num a => a]` which is a [very different type](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/impredicative_types.html), beyond the scope of this guide.

Similarly, if a function which requires a constraint is used as part of a larger program, that constraint floats to the top:

```hs
complexFunction :: Eq a => a -> ...
complexFunction x = let y = notEqual (x, x) in ...
```

Because `notEqual` is called on `(x, x)`, `x` must be of a type that is an instance of `Eq`. 

The compiler will reason in this way, even if you don't write down the type signature yourself.



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

In the typeclass `Eq`, types which are instances are normal types like `Int`, `Bool`, or `Either Int Bool`, i.e. types with [kind](/basics/types/#types-for-types) `*`.


??? Note
    Accordingly, a type signature like 

    ```hs
    (==) :: Eq a => a -> a -> Bool
    ```

    can be [more explicitly stated as](/basics/types/#universal-quantification-for-other-kinds-than):

    ```hs
    (==) :: forall (a :: *). Eq a => a -> a -> Bool
    ```

However, for many important typeclasses, the types which are instances have other kinds, like `* -> *`. For instance:

```hs
class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b
```

This means that `[]` or `Maybe` are candidates to be instances of `Functor`, but `Int`, `Bool`, (or even `[Int]`) are not.