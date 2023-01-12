---
comments: true
---

How do I give optional arguments to a function? For example, in

```hs
shiftByFour :: Bool -> Int -> Int
shiftByFour flag x = if flag then x + 4 else x - 4
```

I want the `Bool` flag to be optional, and to have a default value.

## Answer

In a literal sense, you cannot have an optional argument. 

The way to do this is to make a function take a `Maybe Bool` instead of a `Bool`. For example:

```hs
shiftByFour maybeFlag x = case maybeFlag of 
    Nothing -> supply-some-default-behavior
    Just flag -> if flag then x + 4 else x - 4
```

!!! Summary
    Instead of optionally having an argument, have a non-optional argument which has an optional type.

