-- A very simple-minded parser for C declarations of the following syntax:
--   "const"? type-specifier ("*" "const"?)* identifier ("[" number "]")?
module DeclarationParser
  ( parse
  ) where

import Control.Monad
import Data.Char
import Text.ParserCombinators.ReadP

parse :: String -> Either String (String, Int)
parse s =
  case readP_to_S parseDeclaration s of
    [(x, "")] -> Right x
    _ -> Left $ "could not parse \"" ++ s ++ "\""

parseDeclaration :: ReadP (String, Int)
parseDeclaration = do
  optionalConst
  typeSpec <- parseTypeSpecifier
  pointers <- many' (token "*" >> optionalConst)
  _ <- parseIdentifier
  a <- parseArray
  skipSpaces
  return (typeSpec, length pointers + a)

optionalConst :: ReadP ()
optionalConst = option' () (void (token "const"))

parseTypeSpecifier :: ReadP String
parseTypeSpecifier =
  choice'
    [ token "void" >> return "()"
    , token "float" >> return "CFloat"
    , token "double" >> return "CDouble"
    , token "int32_t" >> return "Int32"
    , token "int64_t" >> return "Int64"
    , do c <- option' "CChar" (token "signed" >> return "CSChar")
         choice'
           [ token "char" >> return c
           , token "short" >> return "CShort"
           , token "int" >> return "CInt"
           , token "long" >>
             choice' [token "long" >> return "CLLong", return "CLong"]
           ]
    , do _ <- token "unsigned"
         choice'
           [ token "char" >> return "CUChar"
           , token "short" >> return "CUShort"
           , token "int" >> return "CUInt"
           , token "long" >>
             choice' [token "long" >> return "CULLong", return "CULong"]
           ]
    , token "struct" >> parseIdentifier >> return "()" -- Hmmm...
    , token "GLvoid" >> return "()" -- glGetPerfQueryDataINTEL still mentions this
    , parseIdentifier
    ]

parseIdentifier :: ReadP String
parseIdentifier = do
  skipSpaces
  x <- satisfy (\c -> isAlpha c || c == '_')
  xs <- munch (\c -> isAlphaNum c || c == '_')
  return (x : xs)

parseArray :: ReadP Int
parseArray =
  choice'
    [ do _ <- token "["
         skipSpaces
         _ <- munch1 isDigit
         _ <- token "]"
         return 1
    , return 0
    ]

token :: String -> ReadP String
token s = skipSpaces >> string s

-- deterministic versions
choice' :: [ReadP a] -> ReadP a
choice' = foldr (<++) pfail

option' :: a -> ReadP a -> ReadP a
option' x p = choice' [p, return x]

many' :: ReadP a -> ReadP [a]
many' = option' [] . many1'

many1' :: ReadP a -> ReadP [a]
many1' p = liftM2 (:) p (many' p)
