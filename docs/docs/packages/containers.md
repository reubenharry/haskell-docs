---
comments: true
---

A number of useful data structures are defined in the `containers` package.

## Maps

Like a Python dictionary, this has unique (and [orderable](/typeclasses/survey/#ord)) keys, and arbitrary values. These must be of a single type.

```hs
import qualified Data.Map as M

> hasRedHat = M.fromList [("John", True), ("Sally", False), ("Jane", True)]
> hasRedHat
fromList [("Jane",True),("John",True),("Sally",False)]
> :t hasRedHat
hasRedHat :: M.Map String Bool
> :t M.fromList
M.fromList :: Ord k => [(k, a)] -> M.Map k a

-- look up
> M.lookup "John" hasRedHat 
Just True
> M.lookup "Jim" hasRedHat
Nothing
```

<!-- 1. Alternatively use the `at` [lens](/packages/lens) -->


## Sets

```hs title="repl example"
> import qualified Data.Set as S
> S.fromList [1,3,2,1,5,4,3]
fromList [1,2,3,4,5]
```