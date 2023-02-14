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
> equivalentNotEven = not . even -- (2)!
> equivalentNotEven 3
True
> equivalentNotEven 4
False
> test equivalentNotEven
True

-- even more direct!
> test (not . even) -- (3)!
True
```

1. Takes a function of type `Int -> Bool` and tests if it returns `False` for `10` even numbers.

2. See [this section](/thinkingfunctionally/hof/#pointfree-code)

3. It's not even necessary to assign a variable name to the input function at all: just pass in `not . even` as an argument.

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

!!! Note

    A common use case for functions returning functions is [currying](/basics/functions/#currying). Relatedly, there are functions `curry` and `uncurry`:

    ```hs title="repl example"
    > conjunction x y = x && y
    > :t conjunction
    conjunction :: Bool -> Bool -> Bool
    > conjunction True False
    False

    (uncurry conjunction) :: (Bool, Bool) -> Bool
    > (uncurry conjunction) (True, False)
    False

    > conjunction2 (x, y) = x && y
    > :t conjunction2
    conjunction2 :: (Bool, Bool) -> Bool
    > conjunction2 (True, False)
    False
    > :t (curry conjunction2)
    (curry conjunction2) :: Bool -> Bool -> Bool
    > (curry conjunction2) True False
    False
    ```

## Composition

`.` chains together (or *composes*) functions:

```hs title="repl example"

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

Or in its [general](/basics/types/#universal-types) form:

```haskell
(.) :: (b -> c) -> (a -> b) -> (a -> c)
```

### Pointfree code

Instead of writing `func x = not (even x)`, one can write `func = not . even`, which avoids having to name a variable `x` at all. 



=== "pointful style"

    ```haskell
    import Graphics.Gloss.Data.Picture -- (1)!
    picture :: Picture
    picture = 
        rotate 90
        $ translate 20 20
        $ scale 30 30
        circle 2
    ```

    1. Requires the `gloss` package.

=== "pointfree style"
    
    ```haskell
    import Graphics.Gloss.Data.Picture -- (1)!
    picture :: Picture
    picture = transform (circle 2) where 
        transform =
            rotate 90
            . translate 20 20
            . scale 30 30
    ```

    1. Requires the `gloss` package.

Using `flip`:

=== "pointful style"

    ```haskell
    > threeMinusN n = subtract n 3 
    > threeMinusN 6
    -3

    -- or (re the above example)
    > translateXBy n = translate n 0
    ```


=== "pointfree style (with `flip`)"
    
    ```haskell
    > threeMinusN = flip subtract 3
    > threeMinusN 6
    -3

    -- or (re the above example)
    > translateXBy = flip translate 0
    ```


## Map, fold, scan and zip

### Map

```haskell
map :: (a -> b) -> ([a] -> [b])  -- (1)!
```

1. Your repl will display: (a -> b) -> [a] -> [b], leaving the brackets implicit. 

`map f ls` gives the same result as the Python list comprehension `[f(x) for x in ls]`. That is, it applies a function `f` to each element of a list.


??? Info
    In Haskell, one can also write a list comprehension, as: `[f x | x <- list]`.

Note that `map`'s type [ranges over all types](/basics/types/#universal-types) `a` and `b`. This means that it can change the type of the values of the list.

```haskell
> map (> 5) [1..10]
[False,False,False,False,False,True,True,True,True,True]

> data Piece = Bishop | Knight deriving Show
> map show [Bishop, Knight, Knight]
["Bishop","Knight","Knight"] 
```

### Folds


```hs title="repl example"
> foldr (+) 0 [1..10]
55
> :t foldr
foldr :: (a -> b -> b) -> b -> [a] -> b -- (1)!
```

1. Actually, the repl will give a more general type: `#!hs foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b`. This generalizes `foldr` from lists to any kind of "container" type that is an instance of the `Foldable` typeclass. 

The first argument (here `(+)`) is a function of type `a -> (b -> b)` (or here specifically: `Int -> (Int -> Int)`) to combine the list elements. The second argument (here `0`) is an initial value, to be returned if the input list is the empty list `[]`. The third argument is the input list to be folded, here `[1,2,3,4,5,6,7,8,9,10]`.

This is by no means restricted to numerical code:

```hs
data Piece = Bishop | Knight | Rook deriving Show
findBestPiece = foldr best Bishop [Bishop, Knight, Rook, Bishop] 
    
    where
    
        best piece1 piece2
            | value piece1 >= value piece2 = piece1
            | otherwise = piece2

        value piece = case piece of
            Bishop -> 3
            Knight -> 3
            Rook -> 5
```

!!! Tip
    In Haskell, it is often preferable to rely on functions like `foldr` instead of writing explicit recursion yourself.

    === "Explicit recursion"
        ```haskell
        sumList (x:xs) = x + sumList xs
        sumList [x] = x
        ```

    === "Using higher-order function"

        ```haskell
        sumList = foldr 0 (+) -- (1)!
        ```

        1. In fact, Haskell already provides this function, and calls it `sum`.

    This is for two reasons:

    1. It avoids buggy code. For example, `sumList` fails on the empty list `[]`.
    2. It is easier to understand. Explicit recursion can create "goto" like control flow.

    Many programs can be expressed as folds (or unfolds!) over lists or other data structures, and Haskell has a range of [intermediate](https://hackage.haskell.org/package/foldl) and [advanced](https://hackage.haskell.org/package/recursion-schemes) libraries to write time/space efficient one-pass folds over complex data.

### Unfolds

```hs title="repl example"
import Data.List
> unfoldr (\x -> if x > 20 then Nothing else Just (even x, x + 3)) 0
[True,False,True,False,True,False,True]
```    

!!! Tip

    Use [laziness](/laziness/laziness/#infinite-data) to unfold an infinite structure and then fold it back.

    ```hs title="repl example"
    > let evens = unfoldr (\x -> Just (x + 2, x + 2)) 0 -- (1)!
    
    > foldr (+) 0 (take 10 evens)
    110

    > any (>10) evens -- (2)!
    True
    ```

    1. An infinite list of even numbers.
    2. `any` is really just a fold, and can be defined in terms of `foldr`.


### Scans

```hs title="repl example"
> scanl (+) 0 [1,1,1,1]
[0,1,2,3,4]
```

!!! Tip
    This corresponds to code that you would write with an accumulator in a non-functional language.

### Illustrative examples

Under :construction:

