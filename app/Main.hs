-- Adapted from Write You A Haskell
module Main where

import Simple.Parser (parseProg)

import Control.Monad.Trans
import System.Console.Haskeline

process :: String -> IO ()
process input =
  case parseProg input of
    Left err -> do
        putStrLn "Parse Error:"
        print err
    Right ast -> 
        print ast

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop = do
        minput <- getInputLine "Happy> "
        case minput of
            Nothing -> outputStrLn "Goodbye."
            Just input -> liftIO (process input) >> loop