todo: product type

You can make your own types like this:

```haskell
data Piece = Bishop | Knight | King
```

!!! Note
    `Piece` is a type*, but `Bishop`, `Knight` and `King` are values belonging to that type

## 

You can also write:

```haskell
data Piece = Bishop Bool | Knight Bool | King Bool
```

In this definition, a value of type `Piece` would look like `Bishop True`, `Bishop False`, etc. This Boolean flag could be used to represent whether a piece is white or black.

Instead, one could also make a `Color` type and use that, for extra clarity: 

```haskell
data Color = Black | White
data Piece = Bishop Color | Knight Color | King Color
```

## Parameterizing a type

One can also create types which take another type as a parameter:

```haskell
data Piece c = Bishop c | Knight c | King c
```

Here, `Piece Bool` would amount to the first definition above, and `Piece Color` to the second.

## todo

a more complex type 

data Position ...
data Blah a b = ChessPiece (Piece a) | Square Position Rank 

## todo

type synonyms 
newtype 