# Under :construction:


---
comments: true
---

*Parser combinators* are a technique for parsing. 

`megaparsec` (and a more restrictive but faster library, `attoparsec`) are industrial strength parser combinator libraries in Haskell.

## External resources

This [tutorial](https://markkarpov.com/tutorial/megaparsec.html) explains how to use `megaparsec` in detail.

This [tutorial](https://smunix.github.io/dev.stephendiehl.com/fun/002_parsers.html) explains the design and implementation of parser combinators.

## What are parsers?


```hs title="A very simple parser implementation"
import Data.Char (digitToInt, isDigit)

type Parser a = [Char] -> Maybe (a, [Char]) -- (1)!

digit :: Parser Int -- (2)!
digit (x:xs) 
  | isDigit x = Just (digitToInt x, xs) 
  | otherwise = Nothing
digit [] = Nothing
```

1. A `Parser` (parameterized by some type `a`), takes a sequence of characters to be parsed, and **either** fails **or** returns a parse result from some initial segment of the sequence and the remaining sequence with that segment removed.

2. `digit` is a `Parser` parameterized by `Int`. It will try to strip a numeral (`1`, `2`, etc..) from the input sequence, and if the sequence does not begin with a digit, it will fail (i.e., return `Nothing`).

For example:

```hs title="repl example"
Just (8,"")
> digit ['4']
Just (4,"")
> digit ['a']
Nothing
```

## What are combinators

Parser combinator libraries provide not just simple parsers like the above, but the ability to combine parsers to build more complex ones.

A simple example is a function `sequenceParsers :: Parser a -> Parser b -> Parser (a,b)` which takes two parser, runs one and then the next on an input sequence, and returns the pair of results (and the remaining string).



<!-- ```hs
sequenceParsers :: Parser a -> Parser b -> Parser (a,b)
sequenceParsers parser1 parser2 inputText = 
  case parser1 inputText of
        Just (result1, remainingText) -> 
            case parser2 remainingText of 
                Nothing -> Nothing
                Just (result2, finalText) -> Just ((result1, result2), finalText)
        Nothing -> Nothing
``` -->

<!-- To make this approach useful, we need to  -->

<!-- examples of megaparsec usage with lexer, example with another monad: list -->







