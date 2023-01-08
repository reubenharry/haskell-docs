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

Another approach to taking multiple arguments is shown here:

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

```haskell
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

because the line `exampleFunc (Right _) = 0 ` would be matched before the final line was reached.

