A Haskell program won't compile unless the types are consistent, and no coercion of types will take place automatically:

```haskell
intToBool :: Int -> Bool
intToBool x = x > 5

-- this line won't type check!
badInput = intToBool True
```

`badInput` won't compile, because it fails to *typecheck*: it tries to apply `intToBool` to a `Bool`, but `intToBool` takes an `Int` as input.

The compiler will provide an error which tells you this:

![Type checking](/img/typecheck.png)


### Why this is useful

In conjunction with Haskell's [ability to create custom types](/basics/createdata), static typing allows you to enforce conceptual distinctions, which can vastly increase code safety and understanding:

```haskell
data ChessSquare = Square Int Int
data Color = Black | White deriving Show

squareColor :: ChessSquare -> Color
squareColor (Square i j) 
    | even (i+j) = White
    | otherwise = Black

correct =  squareColor (Square 2 4)

-- this line won't type check, and your program won't compile!
incorrect = squareColor (2,4)
```

`incorrect` won't compile, because it fails to *typecheck*: it tries to apply `squareColor` to a tuple `(Int, Int)`, but `squareColor` takes a `ChessSquare` as input.

Even though a `ChessSquare` is a pair of integers "under the hood", it *represents* a square on a chess board, and your code (such as `squareColor`) will respect this distinction.
 
This is particularly useful if you limit the ways to create or transform a `ChessSquare`, see todo (link to case study example)

## How to debug a type error

The most general strategy for debugging a type error (aside from carefully reading the error message) is to replace expressions in your program with `undefined`.

For example

```hs
example = fst (intToBool 4)
```

will fail to typecheck. One could first replace `4` with `undefined`:

```hs
example = fst (intToBool undefined)
```

This will still fail to typecheck, so the problem is not the argument of `intToBool`. 

As a next step, replace a larger expression:

```hs
example = fst (undefined)
```

This will typecheck, so you know that the problem is the output of `intToBool`. (More specifically, `fst` expects a tuple, but gets a `Bool`.)

Once you have removed the error, you can ask the compiler for the type of the expression you replaced with `undefined`. See the next section for details.