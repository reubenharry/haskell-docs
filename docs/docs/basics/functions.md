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

todo:
- multiple arguments
- currying
- output type can't depend on input
- pattern matching