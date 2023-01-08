It is common to see type definitions like the following:

```haskell
data ChessPiece = ChessPiece Int Int
```

The first occurrence of `ChessPiece` is a **type**, but the second is a value, namely a function of type `Int -> Int -> ChessPiece`. 