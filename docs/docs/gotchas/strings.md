---
comments: true
---

Haskell has a legacy type for text called `String`, which is defined as `[Char]`, i.e. a list of characters. This is almost never a good representation of text, and should be avoided. Instead, use `Text` from the module `Data.Text`, after adding the package `text` to your [requirements](/gettingstarted/versions/#installing-packages). 

Normally, `Text` is used in conjunction with the extension

```haskell
{-# LANGUAGE OverloadedStrings #-}
```

which allows you to write text in the obvious way:

```haskell
exampleText :: Text
exampleText = "blahblah"
```

You may also use `OverloadedStrings` for other text representations, such as `ByteString`.