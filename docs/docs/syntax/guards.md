These are similar to `case` statements, but instead of pattern matching, you give a boolean condition:

```haskell
example :: Int -> Bool
example x 
    | x > 0 && x < 10 = True
    | otherwise = False
```

todo: example of interaction with case:
    chess example