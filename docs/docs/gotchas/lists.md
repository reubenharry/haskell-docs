---
comments: true
---

"Why does `[True, 4, "cat"]` give a type error?"

All elements of a list in Haskell must have the same type. This doesn't apply to tuples, like `(True, 4, "cat")`, which will work.