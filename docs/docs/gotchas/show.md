```hs title="repl example"

> data Piece = Bishop | Rook

> Bishop
"No instance for (Show Piece) arising from a use of ‘print’"
```

Haskell is complaining that it doesn't have know how to serialize the value `Bishop` into a `String` (because `Piece` isn't an instance of the [Show](/typeclasses/survey/#show) [typeclass](/typeclasses/overview)).

**Fix as follows:**

```hs title="repl example"
> data Piece = Bishop | Rook deriving Show
> Bishop
Bishop
```

This instructs the compiler to create the obvious `Show` instance for `Piece`. 

!!! Note
    Haskell cannot derive a `Show` instance for all types - data types containing functions in particular are a problem because there *is* no obvious way to `show` them.