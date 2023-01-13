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
import Control.Monad.Except (runExcept, runExceptT, Except, MonadError (throwError, catchError))
import Text.Megaparsec (runParserT, errorBundlePretty, runParser, (<|>))
import Data.Void (Void)
import Parser




data Command where
  BoardUpdate :: (Board -> Board) -> Command
  DisplayBoard :: Command
  Quit :: Command


data ChessError = ReplError Text | ParseError Text deriving Show

main :: IO ()
main = runInputT defaultSettings $ flip evalStateT initBoard $ forever $ do
    line <- lift requestLine 
    let instruction = parseInput line
    board <- get
    let result = evaluate board =<< instruction
    case result of
        Right (BoardUpdate update) -> modify update
        Right Quit -> return ()
        Right DisplayBoard -> lift $ outputStr $ T.unpack $ display board
        Left err -> lift $ outputStr $ show err

    where 

        evaluate board instr = flip runReader board $ runExceptT $ case instr of
            ReplInstruction "quit" -> error ""
            ReplInstruction "display" -> pure DisplayBoard
            ReplInstruction _ -> throwError $ ReplError "no such instruction"

        requestLine :: InputT IO Text
        requestLine = do 
            maybeLine <- getInputLine "Welcome to the repl.\n >"
            case maybeLine of 
                Nothing -> requestLine
                Just line -> pure $ T.pack line

        parseInput :: Text -> Either ChessError Instruction
        parseInput inputText = case runParser parser "" inputText of 
          Left err -> Left $ ParseError $ T.pack $ errorBundlePretty @Text @Void undefined 
          Right instr -> Right instr 
          
          
          
        display :: Board -> Text
        display = undefined

