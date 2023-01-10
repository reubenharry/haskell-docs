A Haskell program won't compile unless the types work out, and no coercion of types will take place automatically:

```haskell
intToBool :: Int -> Bool
intToBool x = x > 5

-- this line won't type check!
badInput = intToBool True
```

`badInput` won't compile, because it fails to *typecheck*: it tries to apply `intToBool` to a `Bool`, but `intToBool` takes an `Int` as input.

The compiler will provide an error which tells you this:

![Type checking](/img/typecheck.png)

## How to debug a type error

The most general strategy for debugging a type error (aside from carefully reading the error message) is to replace expressions in your program with `undefined`.

For example

```hs
example = fst (intToBool 4)
```

will fail to typecheck. One could first replace `4` with `undefined`:

```hs
example = fst (intToBool undefined)
```

This will still fail to typecheck, so the problem is not the argument of `intToBool`. 

As a next step, replace a larger expression:

```hs
example = fst (undefined)
```

This will typecheck, so you know that the problem is the output of `intToBool`. (More specifically, `fst` expects a tuple, but gets a `Bool`.)

Once you have removed the error, you can ask the compiler for the type of the expression you replaced with `undefined`. See the next section for details.