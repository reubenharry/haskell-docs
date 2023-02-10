In addition to [unit testing](https://hackage.haskell.org/package/HUnit), Haskell has powerful libraries for [property based testing](/resources/articles/#testing-profiling-and-benchmarking). This is testing where you specify a property you think your program should have, and the testing library tries to find a counterexample. 

!!! Tip
    Much like unit tests, the success of property tests is not a guarantee of your code's correctness, but it can be a extremely effective way to find bugs quickly.


```hs title="repl example"
import Test.QuickCheck -- (1)!

-- Assert that all lists have length 0
> quickCheck (\x -> length x == 0)
*** Failed! Falsified (after 2 tests):    -- (2)!              
[()] -- (3)!

-- Assert that all lists have length 0 or greater
> quickCheck (\x -> length x >= 0)
+++ OK, passed 100 tests.  

-- Assert that for any numbers a and b, a+b is the same as b+a
> quickCheck (\a b -> a + b == b + a)
+++ OK, passed 100 tests

> import Data.Maybe
-- Assert that if the left element of a tuple is not Nothing, neither is the right
> property = (\(x,y) -> isJust x ==> isJust y) -- (4)!
> quickCheck property
*** Failed! Falsified (after 1 test):                  
(Just (),Nothing)

-- sanity check custom sorting function
> import Data.List (sort) -- (5)!
> mkListProperty sortFn (ls :: [Int]) = sortFn ls == sort ls
> badSort = reverse -- (6)!
> quickCheck (mkListProperty badSort)
*** Failed! Falsified (after 5 tests and 3 shrinks):    
[0,1]
```

1. This [requires](/gettingstarted/versions/#installing-packages) the [QuickCheck](https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/Test-QuickCheck.html) package.
2. `QuickCheck` generates lists randomly until it finds a counterexample to your claim, and then simplifies it to a minimal counterexample.
3. In this case, the counterexample is the one element list containing the [unit](/basics/types/#the-unit-type) value `()`, namely `[()]`
4. `==>` is exported by `QuickCheck`; `a ==> b` (read: `a` implies `b`) evaluates to `False` if and only if `a` is True but `b` is `False`. 
5. `sort` is a trusted sorting function from `Data.List`.
6. `badSort` is a bad sorting algorithm: it just reverses its input.

!!! Warning
    Try to avoid universal quantification when not necessary in properties. For example, `#!hs property sortFn ls = sortFn ls == sort ls` really ought to have a type signature, so that `QuickCheck` knows what the type of the elements of the list are, and can generate appropriate examples. Otherwise it will default to a type (usually `()`).

## Custom data types 

Properties involving custom types require that you provide an instance of the `Arbitrary` typeclass for your type, like so:

```haskell
import Test.QuickCheck (Arbitrary (arbitrary), elements, quickCheck)
import Data.List (sort)


data Piece = Bishop | Rook deriving (Eq, Show, Ord)

instance Arbitrary Piece where
    arbitrary = elements [Rook, Bishop] -- (1)!

exampleProperty :: [Piece] -> Bool -- (2)!
exampleProperty ls = sort ls == [Bishop, Rook]

main :: IO ()
main = quickCheck exampleProperty
```

1. `arbitrary` is the function which generates a `Piece`. This implementation says: draw it as random from the list `[Rook, Bishop]`.
2. Because `Piece` has an `Arbitrary` instance, Haskell can [automatically obtain](/typeclasses/overview/#constraint-implication-instances) an `Arbitrary` instance for `[Piece]`, `Maybe Piece`, and so on.

