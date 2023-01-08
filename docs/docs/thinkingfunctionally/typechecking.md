A Haskell program won't compile unless the types work out, and no coercion of types will take place automatically:

```haskell
intToBool :: Int -> Bool
intToBool x = x > 5

-- this line won't type check!
badInput = intToBool True
```

`badInput` won't compile, because it tries to apply `intToBool` to a `Bool`, but `intToBool` takes an `Int` as input.

The compiler will provide an error which tells you this:

![Type checking](/img/typecheck.png)

