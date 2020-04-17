module Main where

import Frontend
import Middleend
import Backend
import Simple.Parser (parseProg)

import Options.Applicative
import Data.Semigroup ((<>))
import Data.List as List
import Data.Either as Either
import System.FilePath.Posix as Posix
import Control.Monad (when)
import qualified Data.Text.Lazy.IO as TL

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
    let parses = zip files (map parseProg contents)
        processSingle n prog =
            case Frontend.pipeline prog of
                Left err ->
                    putStrLn $ n ++ ": Annotation error - " ++ show err
                Right at -> do
                    let eit = Middleend.pipeline steps at
                    when parallelise $
                        let out = "./out/par_" ++ Posix.takeFileName n
                        in writeFile out (Backend.pipelineEncode eit)
                    when graph $ 
                        let drawnGraphs
                                = Backend.pipelineDraw eit
                            drawnGraphsWithName 
                                = map (\(n, t) -> ("./out/" ++ n ++ ".dot", t)) drawnGraphs
                        in mapM_ (uncurry TL.writeFile) drawnGraphsWithName
    mapM_ (\(n, pe) -> 
            case pe of
                Left err -> 
                    putStrLn $ n ++ ": Parse error - " ++ err
                Right prog ->
                    processSingle n prog
          )
          parses
