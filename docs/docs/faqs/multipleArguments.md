How do I give multiple arguments to a function?

## Answer

In a literal sense, functions only take a single argument. However, this argument can be a tuple. 

```hs
multipleArgsFunc :: (Int, Int) -> Int
multipleArgsFunc (i,j) = i + j 
```


Or, the argument can be one of the inputs, and the return value can be a function which takes the next argument:

```hs
multipleArgsFunc :: Int -> (Int -> Int)
multipleArgsFunc = \i -> (\j -> (i + j))
```

Equivalently, but more idiomatically:

```hs
multipleArgsFunc :: Int -> (Int -> Int)
multipleArgsFunc i j = i + j
```

More details [here](/basics/functions/#Currying)

!!! Summary
    Instead of giving multiple arguments, give a single argument with multiple parts.