When I ask for the length of a tuple, I get:

```hs
> length [1,2,3]
3
> length (True, False)
1
> length ([True, False], True)
1
```

`length` works by calling a "folding" function, which relies on the `Foldable` [typeclass](/typeclasses/survey/#foldable). The `Foldable` instance for tuples is defined in such a way that the result of `length` is always `1`.

While this is confusing, it is a consistent behavior.