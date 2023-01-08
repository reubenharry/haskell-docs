It is common to see type definitions like the following:

```haskell
data ChessPiece = ChessPiece Int Int
```

The first occurrence of `ChessPiece` is a **type**, but the second is a **value**, namely a function of type `Int -> Int -> ChessPiece`. 

The name of the type and value don't have to be the same, but it's common to make this choice to avoid having to invent a new name like `MkChessPiece`. 

## Punning and recursive types

There is even more potential for confusion when a type is recursive, so that the type can itself appear on the right hand side of its definition:

```haskell
data BinTree = Leaf Int | BinTree BinTree BinTree
--  type ^             value ^  type ^   type ^
``` 

Here, the second occurrence of `BinTree` is a value, and the rest are types.