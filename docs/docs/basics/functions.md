## Function application

Given a function, such as:

```haskell
add1 x = x + 1
```

we can call it on input as follows:

```haskell
add1 2
> 3
```

One can also write `add1(2)`, but Haskell convention is to avoid brackets where possible.

## Arguments

In Haskell, a function has a single argument, but that argument can be a tuple of multiple values:

```haskell
exampleFunc :: (Int, Int) -> Int
exampleFunc (i1, i2) = i1 + i2
```

## Currying

Another approach to taking multiple arguments, more commonly used, is shown here:

```haskell
exampleFunc :: Int -> (Int -> Int) -- (1)!
exampleFunc i1 i2 = i1 + i2
```

1. By convention in Haskell, `Int -> Int -> Int` means `Int -> (Int -> Int)`, not `(Int -> Int) -> Int`. 

As the type signature indicates, the first argument is an integer, but *the output is a function* which takes a second integer and returns an integer. 

Accordingly, we can apply `exampleFunc` to an integer, say `5`, and obtain an "add 5" function:

```haskell
> :t exampleFunc 4
(Int -> Int)
> (exampleFunc 4) 5 --(1)!
9
```

1. By convention in Haskell, `exampleFunc 4 5` means `(exampleFunc 4) 5`.

This is often referred to as *partial application*. `exampleFunc` would be described as a *curried function*.

!!! Hint
    When you see a function with a type like `#!haskell Type1 -> Type2 -> Type3 -> Type4` (which brackets as: `#!haskell Type1 -> (Type2 -> (Type3 -> Type4))` ), you can think of it as taking `Type1`, `Type2` and `Type3` as inputs, and outputting `Type4`. 

## Partial application for types

The same holds for types:

```hs title="repl example"
> :kind Either
Either :: * -> (* -> *)
> :kind Either Bool
Either Bool :: * -> *
> :kind Either Bool Int
Either Bool Int :: *
```





## Pattern matching

When defining a function, you can *pattern match*:

```haskell
exampleFunc :: Either Int Bool -> Int
exampleFunc (Left i) = i -- (1)!
exampleFunc (Right True) = 1
exampleFunc (Right _) = 0 -- (2)!
```

1. `i` matches any `Int`. It has to be an `Int` because the input has type `Either Int Bool` (`Int` is on the left).

2. The underscore `_` matches `Bool`. It has to be a `Bool` because the input has type `Either Int Bool` (`Bool` is on the right).

Here is how the function behaves:

```haskell
> exampleFunc (Left 6)
6
> exampleFunc (Right True)
1
> exampleFunc (Right False)
0
```

Here, `(Left i)`, `(Right True)` and `(Right _)` are all patterns.

The patterns are matched top-down. For example, if the function were:

```haskell linenums="1"
exampleFunc :: Either Int Bool -> Int
exampleFunc (Left i) = i 
exampleFunc (Right _) = 0 
exampleFunc (Right True) = 1
```

Then:

```haskell
> exampleFunc (Right True)
0
```

because line 3 would be matched before line 4 was reached.

!!! Note
    Pattern matching follows the definition of types, including custom types:

    ```hs title="repl example" hl_lines="4"
    > data Color = White | Black deriving (Show)
    > data PieceType = Bishop | Knight deriving (Show)
    > data Piece = P PieceType Color deriving (Show)
    > getColor (P _ c) = c
    > getColor (P Bishop White)
    White
    ```

!!! Note
    Patterns can be arbitrarily deeply nested, as in:

    ```hs 
    data Color = Black | White
    data Piece = Bishop Color | Knight Color 

    getColor :: Either Bool Piece -> Maybe Color
    getColor (Right (Bishop c)) = Just c
    getColor (Right (Knight c)) = Just c
    getColor (Left _) = Nothing
    ```

    And also with [recursive types](/basics/createdata/#recursive-types)

    ```hs title="repl example" hl_lines="2"
    > data BinTree = Leaf Char | Branches BinTree BinTree
    > leftRightLeaf (Branches (Branches _ (Leaf c))  _) = c
    > tree = Branches (Branches (Leaf 'a') (Leaf 'c')) (Leaf 'b')  
    > leftRightLeaf tree
    'c'
    ```

### Pattern matching lists

This also applies to [lists](/basics/types/#the-list-type):

```hs title="repl example"
> ls@(head:tail) = [1,2,3]
> head
1
> tail
[2,3]
> ls
[1,2,3]
```

### Using @ in patterns

```hs title="repl example"

-- first example
> let whole@(a,b) = ('a', True)
> a
'a'
> b
True
> whole
('a',True)

-- second example
> let whole@(a, left@(b, c)) = ('a', (True, ()))
> a
'a'
> b
True
> c
()
> whole
('a',(True,()))
> left
(True,())
```

```hs
exampleFunc :: (Int, Bool) -> (Int, Bool)
exampleFunc whole@(int,bool) 
    | even int = whole
    | otherwise = (int - 1, not bool)
```
