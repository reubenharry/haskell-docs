Functions in Haskell are *first class values*, meaning they can be passed around like any other data (such are text or numbers), and also bound to names in the same way:

```hs title="repl example"
todo
```



can return functions, as with [currying](), 
    but they can also take functions as input.

## Composition

`.` chains together (or *composes*) functions:

```haskell title="repl example"

import qualified Data.Text as T
:set -XOverloadedStrings

> :t (=='a') -- (1)!
(=='a') :: Char -> Bool
> (=='a') 'b' 
False

> :t T.head 
T.head :: Text -> Char
> T.head "hello"
'h'

> :t ( (=='a') . T.head)
( (=='a') . T.head) :: Text -> Bool
> ( (=='a') . T.head) "hello"
False
```

1. `#!hs (=='a')` is the function which takes a `Char` and returns `True` if it is `a` otherwise `False`.

2. Here also, the repl gives a more general type. What is shown is more specific, but still true.



The composition operator `.` is not a special syntax; it is a function, typically written in infix position like `+` or `*`, with the following type:

```haskell
(.) :: (Char -> Bool) -> (Text -> Char) -> (Text -> Bool)
```

Or in its general polymorphic form:

```haskell
(.) :: (b -> c) -> (a -> b) -> (a -> c)
```

### Pointfree code

todo

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