--
-- UNDER CONSTRUCTION!!
--

module Chess where


data PieceType = Bishop | Rook | Knight | Pawn | King | Queen -- (1)!
data Color = Black | White
data Piece = Piece PieceType Color

data Rank = R Int 
data File = F Int

data SquareState where
  Empty :: SquareState
  HasPiece :: Piece -> SquareState

-- foo = s
mkRank :: Int -> Maybe Rank -- (2)!
mkRank i  
    | inRange i = Just $ R i -- (3)!
    | otherwise = Nothing

    where -- (4)!

        inRange n = n `elem` [1..8]


initBoard :: Board 
initBoard = undefined 

newtype Board where
  Lookup :: (Rank -> File -> SquareState) -> Board
  

-- use flip, const
