{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
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
import Control.Monad.Except (runExcept, runExceptT)


initBoard = undefined 
initBoard :: Board 

data Command where
  BoardUpdate :: (Board -> Board) -> Command
  DisplayBoard :: Command
  Quit :: Command
  
data Instruction = Set Square Piece | CloseRepl | Display
newtype Board where
  Lookup :: (Rank -> File -> SquareState) -> Board
  
main :: IO ()
main = runInputT defaultSettings $ flip evalStateT initBoard $ forever $ do
    line <- lift requestLine 
    let instruction = parseInput line
    board <- get
    let result = evaluate instruction board
    case result of
        Right (BoardUpdate update) -> modify update
        Right DisplayBoard -> lift $ outputStr $ T.unpack $ display board
        Left err -> lift $ outputStr err

    where 

        evaluate = undefined

        -- evaluate :: Board -> ReaderT Board (Except) Command
        -- evaluate instr board = flip runReader board $ runExceptT $ case instr of
        --     CloseRepl -> pure Quit
        --     Display -> pure DisplayBoard

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