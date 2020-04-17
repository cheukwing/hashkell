module Backend (
    pipelineEncode,
    pipelineDraw
) where

import Middleend (EncodingInstructionTable, EncodingInstruction(..))
import Backend.CodeGenerator
import Backend.Drawer
import Simple.Syntax

import qualified Data.Text.Lazy as TL
import qualified Data.Map.Strict as Map

-- pipelineEncode returns the Haskell encoding using all the 
-- given encoding instructions
pipelineEncode :: EncodingInstructionTable -> Code
pipelineEncode = encode

-- pipelineDraw returns the GraphVizDot graphs of all the functions
-- which should be parallelised (have dependency graphs)
pipelineDraw :: EncodingInstructionTable -> [(Name, TL.Text)]
pipelineDraw eit 
    = [ (name, graphToDot dg) | (name, Parallel _ _ dg) <- Map.toList eit ]