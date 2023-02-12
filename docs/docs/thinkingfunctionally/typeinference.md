---
comments: true
---

Haskell is able to *infer* the type of all expressions[^1].

[^1]: There are exceptions, but they involve advanced techniques.

A simple example you can run in the repl:

```haskell
> exampleValue = True
> :t exampleValue
exampleValue :: Bool
```

A more complex example:

```haskell
data ChessPiece = Piece PieceType Color
data Color = Black | White
data PieceType = Bishop | Rook

isPieceWhite = \case
    Piece _ White -> True
    _ -> False

```

Haskell's inference about the type of the whole program is shown here, via the Haskell Language Server, which offers to autocomplete it for you. (If you don't have the Haskell Language Server, do the same in the repl with `:t isPieceWhite`)

![Haskell Language Server](/img/toplevelinference.png)

One can also mouse over any expression in the program:

![Haskell Language Server](/img/mouseover.png)

(If you don't have the Haskell Language Server, do the same by replacing the expression in question with an `_`)

## Type-driven development

The expressive types and automatic inference let you develop a complicated program top-down, by starting with the type signature of the whole program and filling in gaps incrementally. For example:

```haskell hl_lines="6 14"
main :: IO ()
main = runInputT defaultSettings $ flip evalStateT initBoard $ forever $ do
    line <- lift requestLine 
    let instruction = parseInput line
    board <- get
    let result = evaluate instruction board
    case result of
        Right (BoardUpdate update) -> modify update
        Right DisplayBoard -> lift $ outputStr $ T.unpack $ display board
        Left err -> lift $ outputStr err

    where 

        evaluate = undefined
        display = undefined
```


Here, `undefined` stands for a piece of code that is yet to be written. The compiler will typecheck this program, even with the `undefined`s left in, so you can be sure that it is consistent, before proceeding to fill in the gaps.

More importantly, it will infer the type of each `undefined`, like so:

![Inference](/img/typedirected.png)

Especially for more complex programs, the compiler's understanding of the types of unwritten parts of your program can be invaluable.

### Type based refactoring

Because types are expressive in Haskell, and type errors are static, refactoring a codebase can be performed with the help of the compiler:

```hs
type Position = (Double, Double)
```

Here, if we changed the definition of `Position` to `#!hs type Position = (Double, Double, Double)`, we would be shown a list of compiler errors (in the `Problems` tab in VSCode's terminal), which we could then fix one-by-one until the code compiles again.
