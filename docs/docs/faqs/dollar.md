---
comments: true
---

Why is Haskell code full of `$` signs?

```hs
someFunction $ anotherFunction $ aThirdFunction argument
```

## Answer

Read this as:

```hs
someFunction (anotherFunction (aThirdFunction argument))
```

In other words, everything to the right of any `$` is the argument of everything to the left.

It is popular as a way to avoid excessive brackets. See [here](/basics/syntax/#dollar-sign) for more. See also this [Stack Overflow question](https://stackoverflow.com/questions/940382/what-is-the-difference-between-dot-and-dollar-sign).