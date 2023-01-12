---
comments: true
---

Haskell has a built in type for text called `String`, which is defined as `[Char]`, i.e. a list of characters. This is almost never a good representation of text, and should be avoided. Instead, use `Text` from the module `Data.Text`, from the package `text`. 

Normally, `Text` is used in conjunction with the extension

```haskell
{-# LANGUAGE OverloadedStrings #-}
```

which allows you to write text in the obvious way:

```haskell
exampleText :: Text
exampleText = "blahblah"
```