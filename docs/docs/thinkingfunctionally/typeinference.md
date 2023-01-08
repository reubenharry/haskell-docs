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

## Type-driven development

The expressive types and automatic inference let you develop a complicated program top-down, by starting with the type signature of the whole program and filling in gaps incrementally. For example:

```haskell
complicatedFunction :: ChessPiece -> [Int]
complicatedFunction chesspiece = case chesspiece of
    Piece _ Black -> numMovesBlack
    Piece _ White -> numMovesWhite

    where
        numMovesBlack = filter isValidMove allMoves
        numMovesWhite = undefined
        isValidMove = undefined
        allMoves = undefined
```

todo fix ^^

Here, each occurence of `undefined` stands for a piece of code that is yet to be written. The compiler will typecheck this program, even with the `undefined`s left in, so you can be sure that it is consistent, before proceeding to fill in the gaps.

More importantly, it will infer the type of each `undefined`, like so:

todo example

Especially for more complex programs, the compiler's understanding of the types of unwritten parts of your program can be invaluable.