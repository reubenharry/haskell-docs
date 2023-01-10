## Indentation

Haskell is indentation sensitive, like Python. Tabs or spaces are fine.

```hs 
example input = result <> " tree" where
    result = case input of
        True -> "red"
        False -> "black"
```

## Infixing and sections

For operators like `+` or `/` that are written *infix* (i.e. in between their two arguments), Haskell has some syntactic sugar:

```haskell title="repl example"
> 5 / 2
2.5
> (/) 5 2 -- (1)!
2.5
> (/) 2 5
0.4
> (/ 5)  2 -- (2)!
0.4
> (5 /)  2
2.5
```

1. Whether infix or not, the type of `(/)` is `#!haskell Double -> (Double -> Double)`.
2. This is called a "section".

### Infixing in types

Similarly, `->` is a *type-level* infix operation: `a -> b` can be written `(->) a b`.

As this suggests, `->` is like `Either` in that it is a function on types:

```hs title="repl example"
> :kind (->)
(->) :: * -> (* -> *)
> :kind (->) Bool
(->) Bool :: * -> *
> :kind (->) Bool Bool 
(->) Bool Bool :: *
```

## Bracketing

Haskell tends to avoid brackets when possible.

Expressions like `4 * 3 + 5` take a default bracketing (given when the operators were defined).

Functions application is left-associative: 

- `f x y z` means `((f x) y) z`
- `Either Bool Int` means `(Either Bool) Int`

The `->` type is right-associative:

- `A -> B -> C -> D` means `A -> (B -> (C -> D))`

### Dollar Sign

Code like the following is common in Haskell:

```hs
exampleWithDollar = not $ (> 3) 4
```

This is equivalent to:

```hs
exampleWithoutDollar = not ( (> 3) 4)
```

!!! Note
    `$` is just a regular function, used infix, and defined so that `f $ x = f x`.

    For more explanation, see: https://typeclasses.com/featured/dollar.



Dollars can be stacked:

```hs
exampleWithDollar = not $ (> 3) $ head [1,2,3]
```

means the same as:

```hs
exampleWithoutDollar = not ((> 3) (head [1, 2, 3]))
```

!!! Tip
    Whenever you see a `$`, read everything to the right as the input to what is on the left.


## Case-of

Here is an example of a `case _ of` statement:

```haskell hl_lines="8 9 10"
{-# LANGUAGE OverloadedStrings #-}
import Data.Text (Text)

data Color = Black | White
data Piece = Bishop Color | Knight Color | King Color

pieceToText :: ChessPiece -> Text
pieceToText (Piece _ color) = case color of 
    Black -> "black"
    White -> "white"   
```

This is a convenient way to pattern match.


## Guards

These are similar to `case` statements, but instead of pattern matching, you give a boolean condition:

```haskell
example :: Int -> Bool
example x 
    | x > 0 && x < 10 = True
    | otherwise = False
```

## Let-in


```haskell
example :: Int
example = 
    let val1 = 5 
        val2 = val1 + 5
    in val1 + val2 + 1 
```

Let-bindings may be recursive.

```haskell
example :: Int
example = 
    let val1 = 0 : val2
        val2 = 1 : val1
    in val1
```

!!! Hint 

    This gives an infinite list. You can sample from it as follows:

    ```haskell
    > take 20 example
    [0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1]
    ```

    See the section on [laziness](/laziness/laziness) for more information.

## Where

Similar to `let`:

```haskell
example = val1 where
    val1 = 0 : val2
    val2 = 1 : val1
```

See [here](https://wiki.haskell.org/Let_vs._Where) for differences .
