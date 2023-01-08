## Case-of

Here is an example of a `case _ of` statement:

```haskell
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

Similar to `let`. 

todo: differences

```haskell
example = val1 where
    val1 = 0 : val2
    val2 = 1 : val1
```

## Infixing and sections

For operators like `+` or `/` that are written *infix* (i.e. in between their two arguments), Haskell has some syntactic sugar:

```haskell
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

1. Whether infix or not, the type of `(/)` is `Double -> (Double -> Double)`.
2. This is called a "section".