When you write a definition, like:

```haskell
value = True 
```

or 

```haskell
swap (a, b) = (b, a)
```

you cannot later write:

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

This imperative approach isn't usually the natural approach in a functional language. 

!!! Note
    There are simple ways to write code like this, e.g.:

    ```haskell
    todo -- (1)!
    ```

    1. Here, `forM` is a function, not a built-in command.  

    However, there's often a simpler solution that avoids thinking about loops and state altogether.


Instead, you could write:

```haskell
x = sum [0..10]
```

TODO check python and haskell

See the next section for common folds etc
