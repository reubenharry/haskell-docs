---
comments: true
---

Haskell only evaluates an expression if it is needed for the computation as a whole:

```hs title="repl example"
> let x = undefined in x -- (3)!
" *** Exception: undefined"
> let x = undefined in 3 + 5 -- (1)!
8
> let x = undefined in (True || x) -- (2)!
True
```

1. `x` is never used, so is never evaluated, and the error never gets triggered.
2. `x` is never used here either, because if the first argument of `||` is `True`, it returns `True` without evaluating the second.
3. [undefined](/thinkingfunctionally/purity/#caveats) is a value that will throw a runtime error when evaluated.

## Infinite data

A common use case of lazy evaluation is to define an infinite piece of data, and only take a finite part.

```hs title="repl example"
-- first example
[1..]
<program hangs...>
> take 10 [1..] -- (1)!
[1,2,3,4,5,6,7,8,9,10]
> take 10 (filter even [1..])
[2,4,6,8,10,12,14,16,18,20]

-- second example
> let ones = 1 : ones
> take 10 ones
[1,1,1,1,1,1,1,1,1,1]
```

1. `[1..]` is an infinite list of all the integers (`[1,2,3,4...]`), so 


!!! Note
    Infinite lists in Haskell are similar to generators in Python, which can be thought of as the special case of lazy evaluation for lists.

    However, laziness applies to data structures other than lists, and is a pervasive feature of the language.

As a consequence, it is common to build an infinite structure recursively, and only consume a part, such as a finite prefix of this infinite list of prime numbers:

```hs
primes = filterPrime [2..]
  where filterPrime (p:xs) =
          p : filterPrime [x | x <- xs, x `mod` p /= 0]
```



!!! Gotcha
    The consequences of laziness for algorithmic complexity analysis of purity and laziness are wide-ranging. For a good overview, see [this article](https://en.wikipedia.org/wiki/Purely_functional_data_structure#Design_and_implementation)

## Mutual recursion


```hs
func1 = 1 : func2
func2 = 2 : func1
```

Then, `#!hs take 10 func1` gives `[1,2,1,2,1,2,1,2,1,2]`.

## A deeper dive

See [here](/resources/articles/#laziness-and-performance) for more resources on lazy algorithms and the performance implications of laziness.