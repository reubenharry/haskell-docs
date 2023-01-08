How can I write a function which returns one type sometimes and another type other times?

The key is to use `Either`. For example, suppose you want to write a function that returns the square of a number if the number is positive, but otherwise an error message text. Do this:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)

squareWithFailure :: Int -> Either Text Int --(1)!
squareWithFailure i 
    | i >= 0 = Right (i ^ 2)
    | otherwise = Left "squareWithFailure only takes non-negative input"
```

1. It is conventional for a type representing a failure to go on the left side of an `Either`.