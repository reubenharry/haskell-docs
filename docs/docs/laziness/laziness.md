Haskell only evaluates an expression if it is needed for the computation as a whole:

```hs title="repl example"
> let x = undefined in x
" *** Exception: undefined"
> let x = undefined in 3 + 5 -- (1)!
8
> let x = undefined in (True || x) -- (2)!
True
```

1. `x` is never used, so is never evaluated, and the error never gets triggered.
2. `x` is never used here either, because if the first argument of `||` is `True`, it returns `True` without evaluating the second.

## Infinite data

A common use case of lazy evaluation is to define an infinite piece of data, and only take a finite part.

```hs title="repl example"
[1..]
<program hangs...>
> take 10 [1..] -- (1)!
[1,2,3,4,5,6,7,8,9,10]
```

1. `[1..]` is an infinite list of all the integers (`[1,2,3,4...]`), so 

!!! Hint
    Infinite lists in Haskell are similar to generators in Python, which can be thought of as the special case of lazy evaluation for lists.


todo:
    complex example where lazy evaluation percolates, like sort or something

!!! Gotcha
    todo: complexity analysis and laziness: see book