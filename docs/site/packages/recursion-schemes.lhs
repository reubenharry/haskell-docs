```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
import Data.Functor.Foldable
import Data.Fix (Fix(..))
import Data.Text (Text)


data TreeStructure a where
  Leaf :: Text -> TreeStructure a
  Branches :: a -> a -> TreeStructure a 
  deriving Functor

type Tree = Fix TreeStructure

prettyPrint :: Tree -> Text 
prettyPrint = cata \case
    Leaf text -> text
    Branches a b -> "(" <> a <> " " <> b <> ")"

main = print $ prettyPrint $ Fix $ (Branches (Fix $ Leaf "hello") (Fix $ Leaf "world"))

-- >>> 4 + 4
```