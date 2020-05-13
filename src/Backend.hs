module Backend (pipeline) where

import Middleend (EncodingInstructionTable, EncodingInstruction(..))
import Backend.CodeGenerator
import Backend.Drawer
import Context
import Hashkell.Syntax

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Map.Strict as Map

import Debug.Trace (trace)


pipeline :: Context -> EncodingInstructionTable -> IO ()
pipeline ctx
    = if ctxParType ctx == None
        then pipelineDraw
        else pipelineEncode ctx


-- pipelineEncode encodes the encoding instructions into Haskell, and then
-- writes to the file given in the context
pipelineEncode :: Context -> EncodingInstructionTable -> IO ()
pipelineEncode ctx eit = writeFile (ctxOutput ctx) (encode ctx eit)

-- pipelineDraw creates GraphVizDot graphs of all the functions
-- which have dependency graphs, and writes them to a file
pipelineDraw :: EncodingInstructionTable -> IO ()
pipelineDraw eit 
    = mapM_ (uncurry TL.writeFile) ngs
    where
        ngs = [ ("./out/" ++ name ++ ".dot", graphToDot dg) 
                | (name, Parallel _ _ dg) <- Map.toList eit ]