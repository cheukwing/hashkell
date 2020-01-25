-- Adapted from Write You A Haskell
module Main where

import Simple.Parser (parseProg)

import System.Environment
import System.Console.GetOpt
import Control.Monad
import Control.Monad.Trans
import System.Console.Haskeline

data Flag = Interactive | File String
    deriving (Show)

options :: [OptDescr Flag]
options = []
    -- [ Option ['i'] ["interactive"] (NoArg Interactive) "interactive mode"
    -- , Option ['f'] ["file"] (ReqArg File "FILE") "parse a file"
    -- ]

process :: String -> IO ()
process input =
    case parseProg input of
        Left err -> do
            putStrLn "Parse Error:"
            print err
        Right ast -> 
            print ast

parseArgs :: [String] -> IO ([Flag], [String])
parseArgs argv = 
    case getOpt Permute options argv of
        (o, n, []) -> 
            return (o,n)
        (_, _, errs) -> 
            ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: ic [OPTION...] files..."

main :: IO ()
main = do
    (_, fs) <- getArgs >>= parseArgs
    case fs of
        [] -> 
            runInputT defaultSettings interactiveLoop
        files -> 
            processAllFiles files
    where
        interactiveLoop = do
            minput <- getInputLine "Happy> "
            case minput of
                Nothing -> outputStrLn "Goodbye."
                Just input -> liftIO (process input) >> interactiveLoop
        processAllFiles = foldr (\ x -> (>>) (liftIO (readFile x >>= process))) 
                        (putStrLn "Done.")