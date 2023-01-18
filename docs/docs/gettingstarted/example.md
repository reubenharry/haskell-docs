---
comments: true
---

An example Haskell file. This defines a *module* which can be imported by other modules in project.

```hs
{-# LANGUAGE GADTs #-} -- (1)!

module Chess where -- (2)!


-- (3)!
data Piece where 
    Bishop :: Piece
    Rook   :: Piece

isBishop :: Piece -> Bool
isBishop Bishop = True
isBishop _ = False
```

1. Language extensions go here.
2. Module name must match the file name.
3. Code goes here and below.
