module Parser where

import Chess
  ( Color (Black, White),
    File (..),
    Piece (..),
    PieceType (..),
    Rank (..),
  ) -- (13)!
import Control.Applicative (asum, optional)
import Data.Char (digitToInt, isDigit)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (char, space)
import Text.Megaparsec.Char.Lexer (lexeme)
import qualified Data.Map as M -- (14)!
import Witch (into)

type Parser = Parsec Void T.Text -- (15)!

data ChessError = ReplError Text | ParseError Text | Exit deriving Show -- (1)!

-- the result of the parser will be a value of this type
data Instruction where
  Set :: File -> Rank -> Piece -> Instruction
  ReplInstruction :: Text -> Instruction
  deriving (Show)

-- run the parser
parse :: Text -> Either ChessError Instruction
parse line =
  either -- (3)!
    (Left . ParseError . into @Text . errorBundlePretty)
    Right
    (runParser parser "" line)

-- the parser
parser :: Parser Instruction
parser =
  let replCommand (name, instr) = -- (9)!
        const (ReplInstruction instr) <$> name -- (4) (8)
   in asum -- (2)!
        [ replCommand (":q", "quit"),
          replCommand (":r", "reset"),
          replCommand (":d", "display"),
          place
        ] -- (5)!
  where

    -- place (a) white bishop on a4
    place :: Parser Instruction -- (6)!
    place = do -- (12)!
      word "place" -- (7)!
      optional $ word "a"
      piece <- parsePiece
      word "on"
      file <- asum $ parseFile <$> [A .. H] 
      rank <- asum $ parseRank <$> [One .. Eight]
      eof -- (10)!
      return $ Set file rank piece -- (11)!

-- a helper function to add trailing whitespace to a parser
word :: Parser b -> Parser b
word = lexeme (" " >> space)

parsePiece :: Parser Piece
parsePiece = do
  color <- word $ (const White <$> "white") <|> (const Black <$> "black")
  pieceType <-
    word $
      try (const Bishop <$> "bishop")
        <|> try (const King <$> "king")
        <|> try (const Queen <$> "queen")
        <|> try (const Knight <$> "knight")
        <|> try (const Rook <$> "rook")
        <|> try (const Pawn <$> "pawn")
  return (Piece pieceType color)

-- given a Rank, produce a parser that only recognizes that rank
parseRank :: Rank -> Parser Rank
parseRank x =
  const x
    <$> char
      ( case x of
          One -> '1'
          Two -> '2'
          Three -> '3'
          Four -> '4'
          Five -> '5'
          Six -> '6'
          Seven -> '7'
          Eight -> '8'
      )

-- given a File, produce a parser that only recognizes that file
parseFile :: File -> Parser File
parseFile x =
  const x
    <$> char
      ( case x of
          A -> 'a'
          B -> 'b'
          C -> 'c'
          D -> 'd'
          E -> 'e'
          F -> 'f'
          G -> 'g'
          H -> 'h'
      )



