---
comments: true
---

When you write a definition in your code, like:

```haskell
flag = True -- (1)!
```

1. `flag` isn't a keyword, it's just a normal variable name.

or 

```haskell
swap (a, b) = (b, a)
```

you cannot later write in the same codebase:

```haskell
flag = False
```

or 

```haskell
swap (a, b) = (a, b)
```

This is because all Haskell values (including functions) are immutable. To write `a = b` is simply to state that `a` is a name for `b`, and that wherever you see `a` in the code, it can be replaced by `b`.

For example, given the definition `swap (a, b) = (b, a)`, whenever you see `swap (a, b)` in your code, it can be replaced by `(b, a)`.

## Loops and mutation

In Python you could write:

```python
x = 0
for i in range(10):
    x += i
```

This imperative approach isn't usually the natural one in a functional language. 

!!! Note
    There are fairly simple ways to write code like this, e.g.:

    ```haskell
    import Control.Monad (forM)
    import Control.Monad.State (execState, modify)

    loop = flip execState 0 $ forM [0..9] $ \i -> -- (1)!
        modify (+i)
    ```

    1. Here, `forM` and `modify` are just regular functions, not built-in control-flow operators.  

    However, there's often a simpler solution that avoids thinking about loops and state altogether.


Instead, you could write:

```haskell
x = sum [0..9]
```

See [this section](/thinkingfunctionally/hof/#map-fold-scan-and-zip) for how this kind of approach scales to more complex situations.
