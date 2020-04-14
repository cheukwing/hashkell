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

pipelineEncode :: EncodingInstructionTable -> Code
pipelineEncode = encode

pipelineDraw :: EncodingInstructionTable -> [(Name, TL.Text)]
pipelineDraw eit 
    = [ (name, graphToDot dg) | (name, Parallel _ _ dg) <- Map.toList eit ]