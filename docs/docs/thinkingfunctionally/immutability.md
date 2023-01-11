When you write a definition in your code, like:

```haskell
value = True 
```

or 

```haskell
swap (a, b) = (b, a)
```

you cannot later write in the same codebase:

```haskell
value = False
```

or 

```haskell
swap (a, b) = (a, b)
```

This is because all Haskell values (including functions) are immutable. To write `a = b` is simple to state that `a` is a name for `b`. 

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

    1. Here, `forM` is a function, not a built-in command.  

    However, there's often a simpler solution that avoids thinking about loops and state altogether.


Instead, you could write:

```haskell
x = sum [0..9]
```

See the next section for how this kind of approach scales to more complex situations.
