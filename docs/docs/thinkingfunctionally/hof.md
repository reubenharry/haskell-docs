---
comments: true
---

Functions in Haskell are *first class values*, meaning they can be passed around like any other data (such are text or numbers), and also bound to names in the same way:

```hs title="repl example"
> notEven = (\x -> not (even x))
> notEven 3
True
> notEven 4
False
> test notEvenFunc = all (==False) [notEvenFunc (2*n) | n <- [1..10]] -- (1)!
> test notEven

-- a more concise definition
> equivalentNotEven = not . even
> equivalentNotEven 3
True
> equivalentNotEven 4
False
> test equivalentNotEven
True

-- even more direct!
> test (not . even) -- (2)!
True
```

1. Takes a function of type `Int -> Bool` and tests if it returns `False` for `10` even numbers.

2. It's not even necessary to assign a variable name to the input function at all: just pass in `not . even` as an argument.

Functions can also return functions:

```hs title="repl example"
> mkNotEven isEvenFunc = \n -> not (isEvenFunc n)
> (mkNotEven even) 3 -- (1)!
True

-- equivalently
> mkNotEven2 isEvenFunc = not . isEvenFunc
> (mkNotEven2 even) 3
True
```

1. The result of `mkNotEven`, when applied to `even`, is a *function* to tell if a number is odd. 

A common use case for functions returning functions is [currying](/basics/functions/#currying).

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

Instead of writing `func x = not (even x)`, one can write `func = not . even`, which avoids having to name a variable `x` at all. 

!!! Hint

    Here is a point*ful* style:

    ```hs
    Graphics.Gloss.Data.Picture
    picture :: Picture
    picture = 
        rotate 90
        $ translate 20 20
        $ scale 30 30
        circle 2
    ```

    And here is a point*free* style:

    ```hs
    picture :: Picture
    picture = transform (circle 2) where 
        transform =
            rotate 90
            . translate 20 20
            . scale 30 30
    ```

## Map, fold, scan and zip

### Map

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

### Folds

Under :construction:

### Scans

Under :construction:
