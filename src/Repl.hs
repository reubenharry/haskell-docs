--
-- UNDER CONSTRUCTION!!
--

{-# LANGUAGE OverloadedStrings #-}

module Repl where
import Control.Monad (forever)
import System.Console.Haskeline (getInputLine, runInputT, defaultSettings, InputT, outputStr)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (execStateT, MonadState (..), evalStateT, MonadTrans (..), modify)
import Control.Monad (void)
import qualified Data.Text as T
import Chess
import Control.Monad.Reader (runReader, ReaderT (runReaderT))
import Control.Monad.Except (runExcept, runExceptT, Except, MonadError (throwError))


initBoard = undefined 
initBoard :: Board 

data Command where
  BoardUpdate :: (Board -> Board) -> Command
  DisplayBoard :: Command
  Quit :: Command
  
data Instruction where
  Set :: Square -> Piece -> Instruction
  ReplInstruction :: Text -> Instruction

newtype Board where
  Lookup :: (Rank -> File -> SquareState) -> Board
  
data ChessError = ReplError Text deriving Show

main :: IO ()
main = runInputT defaultSettings $ flip evalStateT initBoard $ forever $ do
    line <- lift requestLine 
    let instruction = parseInput line
    board <- get
    let result = evaluate instruction board
    case result of
        Right (BoardUpdate update) -> modify update
        Right DisplayBoard -> lift $ outputStr $ T.unpack $ display board
        Left err -> lift $ outputStr $ show err

    where 

        evaluate instr board = flip runReader board $ runExceptT $ case instr of
            ReplInstruction "quit" -> pure Quit
            ReplInstruction "display" -> pure DisplayBoard
            ReplInstruction _ -> throwError $ ReplError "no such instruction"

        requestLine :: InputT IO Text
        requestLine = do 
            maybeLine <- getInputLine "Welcome to repl"
            case maybeLine of 
                Nothing -> requestLine
                Just line -> pure $ T.pack line

        parseInput :: Text -> Instruction
        parseInput = undefined


        display :: Board -> Text
        display = undefined