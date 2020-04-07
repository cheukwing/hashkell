-- Adapted from Write You A Haskell
module Main where


import System.Environment
import System.Console.GetOpt
import Control.Monad
import Control.Monad.Trans
import System.Console.Haskeline

import CodeGenerator
import Parallelizer
import Simple.Parser (parseProg)

import Data.List as List
import Data.Either as Either
import System.FilePath.Posix as Posix

data Flag = Interactive | File String
    deriving (Show)

options :: [OptDescr Flag]
options = []
    -- [ Option ['i'] ["interactive"] (NoArg Interactive) "interactive mode"
    -- , Option ['f'] ["file"] (ReqArg File "FILE") "parse a file"
    -- ]

{-
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
-}

main :: IO ()
main = do
    files <- getArgs
    contents <- mapM readFile files
    let 
        parses = zip files (map parseProg contents)
        (_noParse, _parsed) = List.partition (Either.isLeft . snd) parses
        noParse = zip (map fst _noParse) $ Either.lefts $ map snd _noParse
        parsed = zip (map fst _parsed) $ Either.rights $ map snd _parsed

        handleNoParse (n, e)
            = putStrLn $ n ++ ": " ++ e
        
        handleParsed (n, p)
            = writeFile out $ generateCode $ createFunctionTable 10000 p
            where out = "./out/par_" ++ Posix.takeFileName n

    mapM_ handleNoParse noParse
    mapM_ handleParsed parsed

    