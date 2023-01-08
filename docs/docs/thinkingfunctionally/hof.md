Functions in Haskell can return functions, as with [currying](), 
    but they can also take functions as input.

```haskell
> :t map
map :: (a -> b) -> ([a] -> [b]) -- (1)!
```

`map` takes a function as input, and returns a function as output. In particular, if you have any types `a` and `b`, `map` will take a function `f :: (a -> b)` and will return a new function `g :: ([a] -> [b])`. `g` takes a list of `a`s and applies `f` to each element to obtain a list of `b`s:

```haskell
> map (+1) [1..10]
[2,3,4,5,6,7,8,9,10,11]

```
1. Your repl will display: (a -> b) -> [a] -> [b], leaving the brackets implicit. 

todo foldr, scanl/r, filter, forever