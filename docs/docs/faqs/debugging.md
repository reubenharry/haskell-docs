---
comments: true
---

How do I debug, since `print` statements change the type?

Because Haskell is pure and functional, the approach of debugging by adding `print` statements is not needed as often. Instead, you can inspect the subcomponents of your program and their types in isolation, and check their behavior in the repl. 

However, when you do need `print` statements, you can use `trace` from `Debug.Trace`, like so:

```hs title="repl example"
> import Debug.Trace (trace)
> let output = take 10 [1..] in trace (("Length: " <> show (length output))) output
Length: 10
[1,2,3,4,5,6,7,8,9,10]
```
