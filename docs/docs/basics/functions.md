---
comments: true
---

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

!!! Note

    One can choose any name for an argument (provided it is alphanumeric and starts with a lowercase letter).

    If you have named something else by the same name elsewhere, that is fine:

    ```hs linenums="1" 
    x = True
    add1 x = x + 1
    ```

    The occurrence of `x` on the right of line 2 refers to the occurrence on the left of line 2, not the occurrence on line 1.

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

> add4 = exampleFunc 4
> add4 5
9

-- or directly
> (exampleFunc 4) 5 -- (1)!
9
```

1. By convention in Haskell, `exampleFunc 4 5` means `(exampleFunc 4) 5`.

This is often referred to as *partial application*. `exampleFunc` would be described as a *curried function*.

!!! Hint
    When you see a function with a type like `#!haskell Type1 -> Type2 -> Type3 -> Type4` (which brackets as: `#!haskell Type1 -> (Type2 -> (Type3 -> Type4))` ), you can think of it as taking `Type1`, `Type2` and `Type3` as inputs, and outputting `Type4`. 

## Partial application for types

The same holds for types and their [kinds](/basics/types/#types-for-types):

```hs title="repl example"
> :kind Either
Either :: * -> (* -> *)
> :kind Either Bool
Either Bool :: * -> *
> :kind Either Bool Int
Either Bool Int :: *
```





## Pattern matching on sum types

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

## Pattern matching on product types

The above example concerns a type that is *either* a `Right True` *or* a `Right False` *or* a  `Left 0`, or a `Left 1`...In other words, it is a [sum type](/basics/createData/#sums).

Pattern matching also works with products (and products of sums, sums of products, and so on):

```hs title="repl example"
> ex = (True, False)
> (b1, b2) = ex
> b1
True
> b2
False

-- with a custom type
> data Entity = Sq Int Bool
> entity = Sq 4 False
> Sq i b = entity
> i
4
> b
False
```

This works also in functions:

```hs title="repl example" hl_lines="4"
> data Color = White | Black deriving Show
> data PieceType = Bishop | Knight deriving Show
> data Piece = P PieceType Color deriving Show
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

    And also used with [recursive types](/basics/createdata/#recursive-types)

    ```hs title="repl example" hl_lines="2"
    > data BinTree = Leaf Char | Branches BinTree BinTree
    > leftRightLeaf (Branches (Branches _ (Leaf c))  _) = c
    > tree = Branches (Branches (Leaf 'a') (Leaf 'c')) (Leaf 'b')  
    > leftRightLeaf tree
    'c'
    ```

### Pattern matching lists

Pattern matching also applies to [lists](/basics/types/#the-list-type), since they are [recursive types](/basics/createData/#recursive-types):

```hs title="repl example"
> ls@(head:tail) = [1,2,3]
> head
1
> tail
[2,3]
> ls
[1,2,3]
```

!!! Tip

    A classic example of returning an error when getting the first element of an empty list:

    ```haskell
    getFirstElement :: [b] -> Either String b
    getFirstElement ( x : _ ) = Right x
    getFirstElement [] = Left "the list is empty"
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

And in a function:

```hs
exampleFunc :: (Int, Bool) -> (Int, Bool)
exampleFunc whole@(int,bool) 
    | even int = whole
    | otherwise = (int - 1, not bool)
```
