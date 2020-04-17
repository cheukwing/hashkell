module Backend.Drawer where

import Middleend (DependencyGraph, DNode(..), DType(..))
import Simple.Syntax
import Backend.CodeGenerator

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.GraphViz as G
import qualified Data.GraphViz.Attributes.Complete as G
import qualified Data.GraphViz.Types as G

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- graphToDot translates a dependency graph into text representing its
-- GraphViz graph
graphToDot :: DependencyGraph -> TL.Text
graphToDot (ns, ds)
    = G.printDotGraph dotGraph
    where dotGraph = G.graphElemsToDot depGraphParams 
            (Map.toList ns)
            (Set.toList ds)


-- depGraphParams sets up the parameters for the GraphViz graph to be drawn
-- with the correct annotations, etc
depGraphParams :: G.GraphvizParams Name DNode DType () DNode
depGraphParams = G.defaultParams {
    G.fmtNode = \(name, node) -> case node of
        Scope         -> [G.toLabel $ "Scope \"" ++ name ++ "\""]
        Expression e  -> [G.toLabel $ name ++ " = " ++ dexprToCode e]
        Conditional e -> [G.toLabel $ name ++ " = If " ++ dexprToCode e]
    , G.fmtEdge = \(_, _, t) -> case t of
        Dep      -> []
        DepThen  -> [G.toLabel "then"]
        DepElse  -> [G.toLabel "else"]
--       DepParam -> [G.style G.dotted]
}