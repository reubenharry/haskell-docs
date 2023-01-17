module Chess where -- (3)!

import Data.Char (toUpper)
import Data.List (intercalate, intersperse)
import Data.Text qualified as T
import Witch (into)


-- (14)!
data PieceType = Bishop | Rook | Knight | Pawn | King | Queen -- (1)!
  deriving (Eq, Show) -- (4)!

data Color = Black | White
  deriving (Eq, Show)

data Piece = Piece PieceType Color -- (2)!
  deriving (Eq, Show)

data File = A | B | C | D | E | F | G | H
  deriving (Eq, Ord, Show, Enum) -- (5)!

data Rank = One | Two | Three | Four | Five | Six | Seven | Eight
  deriving (Eq, Ord, Show, Enum)

data SquareState where -- (7)!
  Empty :: SquareState
  HasPiece :: Piece -> SquareState

-- (15)!
newtype Board where -- (14)!
  Board :: (File -> Rank -> SquareState) -> Board

initBoard :: Board -- (6)!
initBoard = Board $ \f r -> Empty

display :: Board -> T.Text
display (Board boardFunc) =
  into @T.Text $ -- (9)!
    intercalate "\n" $
      map (intersperse '|') $
        group 8 flatBoard
  where

    -- (10)!
    flatBoard =
      [ showSquare (boardFunc file rank)
        | file <- [A .. H],
          rank <- [One .. Eight] -- (8)!
      ]

    showPiece (Piece pieceType color) = letterCase color $ case pieceType of
      Bishop -> 'b'
      King -> 'k'
      Knight -> 'n'
      Queen -> 'q'
      Rook -> 'r'
      Pawn -> 'p'

    showSquare = \case
      -- (11)!
      Empty -> '_'
      HasPiece p -> showPiece p

    letterCase = \case
      Black -> toUpper -- (12)!
      White -> id -- (13)!

-- helper function: split list into chunks of size n
group :: Int -> [a] -> [[a]]
group _ [] = []
group n l = (take n l) : (group n (drop n l))

