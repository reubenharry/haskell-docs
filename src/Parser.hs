--
-- UNDER CONSTRUCTION!!
--

module Parser where

{-# LANGUAGE OverloadedStrings #-}


import Text.Megaparsec
import Chess
import Data.Text (Text)
import Data.Void (Void)

data Instruction where
  Set :: Rank -> File -> Piece -> Instruction
  ReplInstruction :: Text -> Instruction

parser :: Parsec () Text Instruction
parser = 
  ReplInstruction "quit" <$ ":q" 
  <|> undefined