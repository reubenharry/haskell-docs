---
comments: true
---





*Parser combinators* are a technique for parsing. 

`megaparsec` (and a more restrictive but faster library, `attoparsec`) are industrial strength parser combinator libraries in Haskell.

## External resources

This [tutorial](https://markkarpov.com/tutorial/megaparsec.html) explains how to use `megaparsec` in detail.

This [tutorial](https://smunix.github.io/dev.stephendiehl.com/fun/002_parsers.html) explains the design and implementation of parser combinators.

## What are parsers?


```hs title="A very simple version of a parser"
import Data.Char (digitToInt, isDigit)

type Parser a = [Char] -> Maybe (a, [Char]) -- (1)!

digit :: Parser Int -- (2)!
digit (x:xs) 
  | isDigit x = Just (digitToInt x, xs) 
  | otherwise = Nothing
digit [] = Nothing
```

1. A `Parser` (parameterized by some type `a`), takes a sequence of characters to be parsed, and **either** fails **or** returns a parse result from some initial segment of the sequence and the remaining sequence with that segment removed.

2. `digit` is a `Parser` parameterized by `Int` (which is the type of its result). It will try to strip a numeral (`1`, `2`, etc..) from the input sequence. If the sequence does not begin with a digit, it will fail (i.e., return `Nothing`). If it succeeds, it will return an `Int`, **not** a `Char`.

Libraries like `parsec`, `megaparsec` and `attoparsec` provide more sophisticated versions of this idea:

```hs title="repl example"
> import Text.Megaparsec
> import Text.Megaparsec.Char
> :set -XOverloadedStrings

> type Parser = Parsec Void T.Text

-- (1)!
> parseWith parser = putStrLn . (either errorBundlePretty T.unpack ) . parse parser ""

-- (3)!
> aParser = "a" :: Parser T.Text

> parseWith aParser "ab"
a
> parseWith aParser "ba" -- (2)!
1:1:
  |
1 | ba
  | ^
unexpected 'b'
expecting 'a'


abParser = "ab" :: Parser T.Text
> parseWith abParser "ab" 
ab

> parseWith abParser "ba"
1:1:
  |
1 | ba
  | ^^
unexpected "ba"
expecting "ab"
```

1. A helper function for parsing.
2. `megaparsec` generates pretty error messages when it fails.
3. `Parser T.Text` is the type of a parser that returns a result of type `T.Text`




## What are combinators

Parser combinator libraries provide not just simple parsers like the above, but the ability to combine parsers to build more complex ones.

Here are examples from `megaparsec`:

```hs
import Text.Megaparsec

type Parser = Parsec Void Text

-- helper function to run parser
parseAndPrint :: Show b => Parsec Void Text b -> Text -> IO ()
parseAndPrint parser line =
  either
    (putStrLn . errorBundlePretty)
    print
    (runParser parser "" line)

-- parse 'a' then 'b'
abParser :: Parser Text
abParser = "ab" -- (2)!

-- parse 'b' then 'a'
baParser :: Parser Text
baParser = "ba"

-- parse 'ab' then 'ba'
abbaParser :: Parser Text -- (1)!
abbaParser = do 
  ab <- abParser -- (3)!
  ba <- baParser -- (4)!
  return (ab <> ba) -- (5)!

-- parse either 'ba' or 'ab'
baOrabParser :: Parser Text
baOrabParser = baParser <|> abParser -- (6)!
```

This runs as follows:

```hs title="repl example"
> parseAndPrint baParser "ba"
"ba"
> parseAndPrint baParser "ab"
1:1:
  |
1 | ab
  | ^^
unexpected "ab"
expecting "ba"

> parseAndPrint baOrabParser "ab"
"ab"
> parseAndPrint abbaParser "abba"
"abba"
> parseAndPrint abbaParser "baab"
1:1:
  |
1 | baab
  | ^^
unexpected "ba"
expecting "ab"
```

1. `Parser` is a [monad](/typeclasses/survey/#monad), so we can use [do-notation](/basics/syntax/#do-notation), which is very convenient for building complex parsers out of simpler ones.
2. `megaparsec` takes advantage of [OverloadedStrings](/gotchas/strings/), so that `"ab"` is actually a parser.
3. This means: run `abParser` and name the result `ab`.
4. This means: run `baParser` (on what remains after running `abParser` previously) and name the result `ba`.
5. This means: the result of the parser is the value `ab <> ba`.
6. This means: try first `baParser` and if it fails, try `abParser`.

## With custom types

One of the [main appeals](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/) of Haskell's parser combinators is that the output of your parser can be a custom type:

```hs
data PieceType = Bishop | Rook deriving Show
data Color = White | Black deriving Show
data Piece = Piece PieceType Color deriving Show

pieceTypeParser :: Parser PieceType -- (3)!
pieceTypeParser = do
  str <- "bishop" <|> "rook"
  return $ case str of
    "bishop" -> Bishop
    _ -> Rook

colorParser :: Parser Color
colorParser = do
  str <- "white" <|> "black"
  return $ case str of
    "white" -> White
    _ -> Black

pieceParser :: Parser Piece
pieceParser = do
  color <- colorParser
  space -- (1)!
  pieceType <- pieceTypeParser
  return (Piece pieceType color)

optionalColorParser :: Parser Piece
optionalColorParser = do
  maybeColor <- optional colorParser -- (2)!
  pieceType <- pieceTypeParser
  return $ case maybeColor of
    Just color -> Piece pieceType color
    Nothing -> Piece pieceType White
```

1. 0 or more whitespace.
2. `optional` takes `colorParser` and returns a new parser that either parses nothing *or* `colorParser`. The result, appropriately, is a `Maybe Color`.
3. The result type is the custom type just defined above.

Examples:

```hs title="repl example"
> parseAndPrint optionalColorParser "white rook"
Piece Rook White
> parseAndPrint optionalColorParser "rook"
Piece Rook White
> parseAndPrint pieceParser "white rook"
Piece Rook White'
> parseAndPrint pieceParser "rook"
1:1:
  |
1 | rook
  | ^^^^
unexpected "rook"
expecting "black" or "white"
```





