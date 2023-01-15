--
-- UNDER CONSTRUCTION!!
--
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$" #-}

module Parser where



import Text.Megaparsec
import Chess
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec.Char (space)
import Data.Char (isDigit, digitToInt)

data Instruction where
  Set :: Rank -> File -> Piece -> Instruction
  ReplInstruction :: Text -> Instruction

parser :: Ord a => Parsec a Text Instruction
parser = 
      const (ReplInstruction "quit") <$> ":q"
  <|> const (ReplInstruction "reset") <$> ":r" 
  <|> const (ReplInstruction "display") <$> ":r" 
  <|> set

  where 
    set = do
      "set"
      space
      rank <- undefined
      file <- undefined
      "to"
      piece <- undefined
      return $ Set rank file undefined


