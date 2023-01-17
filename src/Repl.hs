

{-# LANGUAGE BlockArguments #-} -- (1)!

module Repl where

import Chess
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Text (Text)
import Data.Text qualified as T
import Evaluator
import Parser
import Text.Megaparsec hiding (parse) -- (7)!
import Witch (into)


main :: IO ()
main = do
    runReplWithBoard $
      displayLine "Welcome!\n\n" >> forever do -- (3)!

        line <- requestLine "> " -- (4)!
        let instruction = parse line -- (5)!
        board <- get -- (6)!
        result <-
          case instruction of
            Left (ParseError err) -> pure err -- (8)!
            Left (ReplError err) -> pure err
            Right instr -> evaluate instr
              `catchError` ( \case -- (9)!
                              ReplError txt -> pure txt
                              err -> throwError err -- (11)!
                          )

        displayLine result -- (10)!
    pure ()

  where

    runReplWithBoard :: StateT Board (ExceptT e IO) a -> IO (Either e a) -- (12)!
    runReplWithBoard = runExceptT . flip evalStateT initBoard -- (13)!

    displayLine :: Text -> StateT Board (ExceptT ChessError IO) ()
    displayLine = liftIO . putStrLn . into @String -- (14)!

    requestLine :: Text -> StateT Board (ExceptT ChessError IO) Text
    requestLine prompt = do
      displayLine prompt
      line <- liftIO getLine
      pure $ into @Text line
