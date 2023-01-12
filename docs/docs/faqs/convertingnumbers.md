---
comments: true
---

"I have a `Float` but want a `Double`". Or: I have a `String` but want a `Text`.


## Answer

Usually one can find the right function via [Hoogle](/gettingstarted/overview/#step-4-hoogle):

![Hoogle](/img/coercion.png)

However, a more convenient option is to use the `Witch` library, which abstracts coercions using a single function, called `into`:


```hs
{-# LANGUAGE TypeApplications #-} -- (1)!

import Witch -- (2)!


float :: Float
float = 4

double :: Double
double = into @Double float -- (3)!

str :: String
str = "hello"

text :: Text
text = into @Text str
```

1. Only needed if you aren't using [GHC2021](/gettingstarted/versions/#extensions).

2. This is a library for performing coercions.

3. The use of the `@Double` here directs helps the type checker infer the output type of `into`. Only needed if you don't supply the type signature `double :: Double`.