Haskell is able to *infer* the type of most expressions.

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

