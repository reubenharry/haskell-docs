module Evaluator where


import Chess ( display, Board(..), SquareState(HasPiece) )
import Control.Monad.Except ( MonadError(throwError) )
import Control.Monad.State ( MonadState(put, get), gets, modify ) 
import Data.Text ( Text ) 
import Parser ( Instruction(..), ChessError(ReplError, Exit) )

evaluate :: (MonadError ChessError m, MonadState Board m) => 
  Instruction -> m Text
evaluate instr = case instr of
  ReplInstruction "quit" -> throwError Exit -- (1)!
  ReplInstruction "display" -> gets display -- (2)!
  Set file rank piece -> do
    (Board boardFunc) <- get -- (3)!
    let newBoard =
          Board -- (4)!
            ( \f r ->
                if f == file && r == rank
                  then HasPiece piece
                  else boardFunc f r
            )
    put newBoard -- (5)!
    return $ display newBoard
  ReplInstruction _ -> throwError $ ReplError "no such instruction"
