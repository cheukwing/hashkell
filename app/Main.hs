-- Adapted from Write You A Haskell
module Main where

import CodeGenerator
import Parallelizer
import Simple.Parser (parseProg)

import Options.Applicative
import Data.Semigroup ((<>))
import Data.List as List
import Data.Either as Either
import System.FilePath.Posix as Posix
import Control.Monad (when)

data Arguments = Arguments
    { file        :: [String]
    , parallelise :: Bool
    , steps       :: Int
    , graph       :: Bool
    }

arguments :: Parser Arguments
arguments 
    = Arguments
        <$> some (argument str 
            ( metavar "FILENAMES..." 
           <> help "The programs to parallelise"))
        <*> switch
            ( long "parallelise"
           <> short 'p'
           <> help "Whether to parallelise the input programs"
            )
        <*> option auto
            ( long "steps"
           <> short 's'
           <> help "The number of steps to set the parallelisation boundary to"
           <> value 1000 
            )
        <*> switch
            ( long "graph"
           <> short 'g'
           <> help "Whether to draw the graph of parallelisable functions"
            )

main :: IO ()
main = process =<< execParser args
    where 
        args = info (arguments <**> helper)
                ( fullDesc
               <> progDesc "Semi-automatic parallelisation of Simple Haskell"
               <> header "unnamed project - semi-automatic parallelisation"
                )

process :: Arguments -> IO ()
process (Arguments files parallelise steps graph) = do
    contents <- mapM readFile files
    let 
        parses = zip files (map parseProg contents)
        handleNoParse n e
            = putStrLn $ n ++ ": " ++ e
        handleParsed n p = do
            when parallelise $
                let out = "./out/par_" ++ Posix.takeFileName n
                in writeFile out $ generateCode $ createFunctionTable steps p
            when graph $ drawGraphs p

    mapM_ (\(n, pe) -> either (handleNoParse n) (handleParsed n) pe) parses
