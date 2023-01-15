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
main = runInputT defaultSettings $ withBoard $ forever $ do -- (1)!
    line <- lift requestLine 
    let instruction = case runParser parser "" line of
          Left err -> Left $ ParseError $ T.pack $ errorBundlePretty @_ @Void err
          Right x -> Right x
    board <- get -- (2)!
    let result = evaluate board =<< instruction
    case result of
        Right (BoardUpdate update) -> modify update
        Right Quit -> return ()
        Right DisplayBoard -> lift $ outputStr $ T.unpack $ display board
        Left err -> lift $ outputStr $ show err

    where 

        withBoard = flip evalStateT initBoard

        
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

          
          
          
