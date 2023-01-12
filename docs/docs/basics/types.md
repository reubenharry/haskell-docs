
## The type of Booleans

`True` is a value in Haskell. Its *type* is `Bool`. In Haskell, we can state this as:

=== "In a repl"

    ```haskell
    True :: Bool -- (1)!
    ```

    1.  Read "X :: Y" as: "the value X has the type Y"

=== "In a file"

    ```haskell
    example :: Bool
    example = True
    ```

Similarly, 

```haskell
False :: Bool
```

!!! Note
    In Haskell, everything from simple values like `True` to complex programs have a unique type. 

!!! Tip
    Haskell types can be quite complex. To understand a type, always ask: what do the values belonging to this type look like?

    For example, the values belonging to `Bool` are `True` and `False`.

## The type of integers

`Int` is a type for integers, as in:

```haskell
5 :: Int
```

??? Gotcha
    `5` can have a more general type in Haskell. See [here](/faq/numbers.md)

## The type of real numbers

There are several available options. A good general choice is `Double`:

```haskell
5.0 :: Double
```

## The type of text

`Char` is the type of single characters:

```hs title="repl example"
> :t 'a'
'a' :: Char

> :t 'b'
'b' :: Char
```

`Text` is the type of sequences of characters:

```haskell
{-# LANGUAGE OverloadedStrings #-} --(1)!
import Data.Text (Text)

exampleText :: Text 
exampleText = "hello world!"
``` 

1. See [here](/gotchas/strings) for why this extension is needed.




<!-- Haskell's type system is such an important feature, and so useful for understanding the language, that it is a good place to begin.

Every expression (that includes all programs) in the language has a unique type. -->


## Functions

A function in Haskell means the same as a function in mathematics: it takes an input and produces an output. The type of the function depends on the type of the input and the type of the output. 

=== "In a repl"

    ```haskell
    (\x -> x > 3) :: (Int -> Bool)
    ```

=== "In a file"

    ```haskell
    exampleFunction = (\x -> x > 3) :: (Int -> Bool)
    ```

!!! Note
    In Python, this would be written: `lambda x: x > 3`

We can also define functions without the lambda syntax, like so:

```haskell
exampleFunction :: Int -> Bool
exampleFunction x = x > 3
```

!! Note
    `Int` and `Bool` here are parameters of the type `Int -> Bool`. One can obtain a different type by changing these parameters,  e.g. `Text -> Int`.

## Product types (tuples)

Pairs of values are themselves values. For example `(True, False)` has type `(Bool, Bool)`:

```haskell
(True, False) :: (Bool, Bool)
```

!!! Note
    `(Bool, Bool)` is a type defined in terms of another type, `Bool`. We could change either the left-hand or right-hand type, to get new types, like:
    
    - `(Bool, Int)`
    - `(Int, Bool)`
    - `((Bool, Int), Bool)`


## Sum types

If you have two types, say `Bool` and `Int`, then you can generate a new type which is their *disjoint union*, called `Either Bool Int`. 

```haskell
(Left True) :: Either Bool Int -- (1)!
(Left False) :: Either Bool Int -- (2)!
(Right 3) :: Either Bool Int
(Right 7) :: Either Bool Int
```

1.  `Left` is a function which takes `True` as an argument. In other languages, this might be written `Left(True)`

2.  `Right` is a function which takes `True` as an argument. In other languages, this might be written `Right(True)`

!!! Note
    `Left` and `Right` are functions. 

    ```haskell
    Left :: Bool -> Either Bool Int --(1)!
    Right :: Int -> Either Bool Int
    ``` 

    1. Actually, the type is more general: `forall a. a -> Either a Int`. See the section on polymorphism.

### Maybe

A closely related type is `Maybe`, which in other languages is sometimes called `Optional`:

```hs title="Some values and their types"
Just True :: Maybe Bool -- (1)!
Just 5 :: Maybe Int
Nothing :: Maybe Bool
-- Also true:
Nothing :: Maybe Int -- (2)!
```

1. `Just` is a function of type `Bool -> Maybe Bool`.

2. The most general type of `Nothing` is `forall a. Maybe a`: see the section on [Universal types](/basics/types/#universal-types).



## The unit type

The type `()` contains a single value, which is also written `()`.

!!! Note
    Conceptually, `Maybe X` is the same as `Either () X` (for any type `X`).

!!! Warning
    This practice of writing a type and a value with the same symbol is known as [punning](/gotchas/punning), and is quite widespread in Haskell. Be sure, when reading `() :: ()`, to understand that the `()` on the left is a *value* and the `()` on the right is a *type*.

## The empty type

`Void` is the type with *no* values. It can be useful, but at an introductory level is fairly rare. 

## The list type

The type of a list of `Bool`s is written `[Bool]` or `[] Bool`.

The type of a list of `Ints` is written `[Int]` or `[] Int`.

More generally, for *any type `a`*, `[a]` is the type of lists of values of type `a`.

!!! Gotcha

    Lists are homogeneous: [all elements must have the same type](/gotchas/lists).

Write a list as in Python, like `[True, False, True]`. `:` is an operator to append to the front of a list. Examples:

```haskell title="repl example"
> 4 : [3, 1]
[4, 3, 1]
> 4 : []
[4]
> [1..10]
[1,2,3,4,5,6,7,8,9,10]
```

!!! Note
    `[1,2,3]` is just convenient syntax for `1 : (2 : (3 : []))`.

## The IO type

The type `IO Bool` describes a process which can do arbitrary I/O, such as reading and writing to files, starting threads, running shell scripts, etc. The `Bool` indicates that a result of running this process will be to produce a value of type `Bool`. More generally, for any type `a`, `IO a` runs a process and returns a value of type `a`. 

An example:

```hs title="repl example"
import qualified Data.Text.IO as T
> :t getLine
T.getLine :: IO T.Text
```

If run, this will read a line from StdIn and this line will be the value of type `Text` that is produced.

The top level function in a Haskell project is often:

```haskell
main :: IO ()
main = ...
```


## Universal types

Here is an example of polymorphism, or universal quantification over types:

=== "Quantifiers implicit"

    ```haskell
    swap :: (a, b) -> (b, a)
    swap (x, y) = (y, x)
    ```

=== "Quantifiers written"

    ```haskell
    swap :: forall a b . (a, b) -> (b, a) -- (1)!
    swap (x, y) = (y, x)
    ```

    1. You'll need the extension `ExplicitForAll` to enable this.


Read this type as saying: for **any** type `a`, and **any** type `b`, this function will take a pair of values, one of type `a` on the left,  and one of type `b` on the right, and give back a pair in the other order.

Specific types are always uppercase, but a variable ranging over types like `a` and `b` above are always lowercase.

!!! Note
   "any type" really means *any* type. That includes `Bool`, `Int`, `Text`, `[Bool]`, `[(Bool, Int)]`, functions like `(Int -> Bool)` or `(Int -> Int) -> Bool`, custom types you defined (e.g. `ChessPiece`), `Either Bool [Int]`, `IO Int`, and so on.

!!! Tip
    Polymorphic types are not like `Any` in Python. For example, the Boolean negation function `not :: Bool -> Bool` does not also have the type `a -> a`.

    In `forall a. (a, b) -> (b, a)`, both occurrences of `a` must be the same, and both occurrences of `b` must be the same. so `(Bool, Int) -> (Int, Bool)` or `(Text, Double) -> (Double, Text)`, but not `(Bool, Int) -> (Double, Text)`. 
    
    For this reason, the only function that has type `forall a. a -> a` is the identity function (written `id`), because that is the only operation you can be sure works for *every* input type.

    And **no** function has the type `forall a b. a -> b`, because that function would need to be able to take an input of any type, and return an output of any type.

### How to use

If you have a function with a polymorphic type as *input*, you can always call it on any particular types. For example:

```hs title="repl example"
> let swap (a,b) = (b,a)
> swap (4, True)
(True,4)
> swap ('a', 3)
(3,'a')
```

If you have a non-function value of a polymorphic type, like [undefined](/thinkingfunctionally/purity/#caveats) `:: forall a . a` , you may use it as the argument to *any function*.

```hs title="repl example"
> :t not
not :: Bool -> Bool
> :t not undefined
not undefined :: Bool
> 
```

### Usage with parametrized types

Universally quantified types can appear as the parameters of other types:

```hs
getLeft :: Either a b -> Maybe a
getLeft (Left x) = Just x
getLeft (Right _) = Nothing
```

The universally quantified `a` and `b` indicate that `getLeft` is only manipulating the structure of the input, but nothing more. For example, if a function like `not` was called on `x`, then `a` could no longer be universally quantified:

```hs hl_lines="2"
getLeft :: Either Bool b -> Maybe Bool
getLeft (Left x) = Just (not x)
getLeft (Right _) = Nothing
```
 

## Types for types

Types themselves have types, sometimes known as *kinds*. 

```hs title="repl example"
> :kind Bool
Bool :: * -- (1)!
> :kind Int
Int :: *
> :kind (Either Bool Int)
Either Bool Int :: *


> :k Either
Either :: * -> (* -> *) -- (2)! 
> :k (Either Bool)
Either Bool :: (* -> *) 
> :k (Either Int)
Either Int :: (* -> *) 

> :k [Bool]
[Bool] :: *
> :k (Bool, Int)
(Bool, Int) :: *

> :k []
[] :: * -> *
```
1. `*` is the *kind* for all types that can have values, like `Bool`, `Either Bool Int`, `[Bool]` and so on.

2. Consult [this section](/basics/functions/#partial-application-for-types) if this is unclear. Note also that it will be displayed: ` * -> * -> *` by the repl.

!!! Note
    The ability to have types of "higher kinds" (i.e. kinds like `* -> *`, or `* -> * -> *`) is a central feature that makes Haskell's type system more sophisticated than many languages.

    In codebases, it is common to encounter types like `ReaderT` which has kind `* -> (* -> *) -> * -> *` or `Fix` of kind `(* -> *) -> *`

### Universal quantification for other kinds than `*`

!!! Tip
    Make sure to use the `GHC2021` [extension](/gettingstarted/versions/#extensions) or add the language extensions recommended by Haskell Language Server for this section.

In a polymorphic type like `forall a. a`, we can explicitly specify the *kind* of types that the quantifier `forall` ranges over:

```hs
swap :: forall (a :: *) (b :: *) . (a, b) -> (b, a)
swap (x, y) = (y, x)
```

The kind does not need to be `*`. For example, here is the type of `fmap` (see [this section about typeclasses](/typeclasses/survey/#functor)):

=== "With kinds shown explicitly"
    ```hs
    fmap ::forall (f :: * -> *) (a::*) (b::*). Functor f => (a -> b) -> (f a -> f b)
    ```

=== "Without kinds shown explicitly (standard)"

    ```haskell
    fmap :: Functor f => (a -> b) -> f a -> f b
    ```
