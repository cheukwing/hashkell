module Middleend (
    Middleend.DependencyGraph.DependencyGraph,
    Middleend.DependencyGraph.Dependency,
    Middleend.DependencyGraph.DNode(..),
    Middleend.DependencyGraph.DType(..),
    Middleend.DependencyGraph.DExpr(..),
    Middleend.DependencyGraph.DLit(..),
    Middleend.Paralleliser.EncodingInstructionTable,
    Middleend.Paralleliser.EncodingInstruction(..),
    pipeline,
) where

import Frontend (FunctionTable)

import Hashkell.Syntax
import Middleend.Cleaner
import Middleend.Paralleliser
import Middleend.DependencyGraph
import Context

-- pipeline cleans up the given aggregation table by renaming any reused
-- identifier names or names similar to those generated by the paralleliser,
-- and removing any unused identifier definitions.
-- these clean up steps ensure that the encoding instructions generated from the
-- aggregation table are correct
pipeline :: Context -> FunctionTable -> EncodingInstructionTable
pipeline ctx
    = if ctxDrawAll ctx
        then createEncodingInstructionTableAll ctx . cleanup
        else createEncodingInstructionTable ctx . cleanup