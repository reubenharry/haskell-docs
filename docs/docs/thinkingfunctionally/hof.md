Functions in Haskell can return functions, as with [currying](), 
    but they can also take functions as input.

## Composition

`.` composes functions:

```haskell title="repl example"
> :t (+4)
(+4) :: Int -> Int -- (1)!
> :t (> 3)
Int -> Bool -- (2)!
> :t ( (> 3) . (+4))
Int -> Bool
```

1. Actually, the repl gives a more general type: `#!haskell Num a => a -> a`. See [here](/faqs/numbers) for info.

2. Here also, the repl gives a more general type. What is shown is more specific, but still true.



TODO correct and finish

The composition operator `.` is not a special syntax; it is a function, typically written in infix position like `+` or `*`, with the following type:

```haskell
(.) :: (Int -> Bool) -> (Text -> Int) -> (Text -> Bool)
```

Or in its general polymorphic form:

```haskell
(.) :: (b -> c) -> (a -> b) -> (a -> c)
```

## Map

```haskell
> :t map
map :: (a -> b) -> ([a] -> [b]) -- (1)!
```
1. Your repl will display: (a -> b) -> [a] -> [b], leaving the brackets implicit. 

`map f ls` gives the same result as the Python list comprehension `[f(x) for x in ls]`. That is, it applies a function `f` to each element of a list.

??? Info
    In Haskell, one can also write a list comprehension, as: `[f x | x <- list]`.


```haskell
> map (+1) [1..10]
[2,3,4,5,6,7,8,9,10,11]
```

## Foldr

todo foldr, scanl/r, filter, forever