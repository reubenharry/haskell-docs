An example Haskell file. This defines a *module* which can be imported by other modules in project.

```hs
{-# LANGUAGE GADTs #-} -- (1)!

module Chess where -- (2)!


data Piece where -- (3)!
    Bishop :: Piece
    Rook   :: Piece

isBishop :: Piece -> Bool
isBishop = \case
    Bishop -> True
    _ -> False
```

1. Language extensions go here.
2. Module name must match the file name.
3. Code goes here and below.