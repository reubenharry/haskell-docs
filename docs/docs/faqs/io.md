How do I run code with side effects in Haskell, like printing or reading lines?

## Answer

There is a special type, `IO ()`, for this kind of general side effects like printing, and these actions can be sequenced using the `do` syntax shown here: 

```hs
import qualified Data.Text as T

example = IO ()
example = do
    print "Welcome"
    line <- T.getLine
    let result = someComplexFunction line
    print result
```
