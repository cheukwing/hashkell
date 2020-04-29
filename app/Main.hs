module Main where

import Frontend
import Middleend
import Backend
import Hashkell.Parser (parseProg)
import Arguments

import Options.Applicative
import Data.Semigroup ((<>))
import Data.List as List
import Data.Either as Either
import System.FilePath.Posix as Posix
import Control.Monad (when)
import qualified Data.Text.Lazy.IO as TL


main :: IO ()
main = runWithArgs process

process :: Arguments -> IO ()
process args = do
    let ctx = argumentsToContext args
        fs = files args
    contents <- mapM readFile fs
    let parses = zip fs (map parseProg contents)
        processSingle n prog =
            case Frontend.pipeline prog of
                Left err ->
                    putStrLn $ n ++ ": Annotation error - " ++ show err
                Right at -> do
                    let eit = Middleend.pipeline ctx at
                    when (parallelise args) $
                        let out = "./out/par_" ++ Posix.takeFileName n
                        in writeFile out (Backend.pipelineEncode eit)
                    when (graph args) $ 
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
