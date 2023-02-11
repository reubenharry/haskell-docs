---
comments: true
---

## Indentation

Haskell is indentation sensitive, like Python. Tabs or spaces are fine.

```hs 
example input = result <> " tree" where
    result = case input of
        True -> "red"
        False -> "black"
```

## Infixing and sections

Given a function `f`, one can write it *infix* (i.e. in between its two arguments):

```hs title="repl example"
-- treating a prefix function as an infix function
> let subtract x y = x - y
> subtract 6 4
2
> 6 `subtract` 4
2
```

Functions whose names are symbols, like `+`, `$` and `.`, are written infix by default. An order of precedence is defined, to avoid the need for bracketing. For example, `f a . f b` means `(f a) . (f b)`, and similarly, `f a $ f b` means `(f a) $ (f b)`. 



For functions like `+` or `/` that are written by default *infix*, Haskell has some syntactic sugar to convert functions from infix to  *prefix* (before their arguments):

```hs title="repl example"

-- treating an infix function as a prefix function
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

!!! Note 

    `otherwise` is not a keyword, in fact it is just the value `True`:

    ```hs title="repl example"
    > otherwise
    True
    > :t otherwise
    otherwise :: Bool
    ```

    For this reason, `otherwise` will always satisfy the guard, and so is an appropriate catch-all final line.

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

## Do-notation

Do-notation is a syntax for imperative style programming. It can be used in conjunction with the [IO](/basics/types/#the-io-type) type:

```hs
example :: IO ()
example = do
  userInput <- getLine
  let reversed = reverse userInput
  writeFile "file/path" reversed
  print reversed
```

Here, the order of operations is top-down (read line, write file, print), and the `<-` arrow gives a name to the result of an operation (like `userInput`, which is the result of reading from stdIn with `getLine`) which can be used later.

!!! Note
    Do-notation gets converted in the following way:

    === "with do"
        ```haskell
       do 
            x <- m
            f x
        ```
    === "without do"

        ```haskell
       m >>= (\x -> f x)
        ```

    Or for the above example:


    === "with do"

        ```haskell
       example :: IO ()
        example = do
            userInput <- getLine
            let reversed = reverse userInput
            writeFile "file/path" reversed
            print reversed
        ```


    === "without do"

        ```haskell
       example :: IO ()
        example = getLine >>= (\userInput -> 
            let reversed = reverse userInput
            in (writeFile "file/path" reversed >>= 
            (\_ -> print reversed)))
        ```

    As this shows, not only `IO`, but any type `f :: * -> *` which is an instance of `Monad` (and thus implements `>>=`) can be used with do-notation.  For this reason, do-notation is common in Haskell code with many different `Monad` [instances](/typeclasses/survey/#monad).

<!-- (options (===): list, maybe, probability, state, see effects section for more ) -->

    


