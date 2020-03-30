module CodeGenerator where

import Parallelizer
import Simple.Syntax
import DependencyGraph

import Data.List (intercalate)
import qualified Data.Map.Strict as Map

header = "import Control.Parallel\nimport Control.Parallel.Strategies"


generateCode :: FunctionTable -> String
generateCode
    = intercalate "\n\n" . (:) header . map generateCode' . Map.toList
    where
        defnCode n args = unwords (n : args) ++ " = "
        typeCode n      = flip (++) "\n" . (++) (n ++ " :: ") . intercalate " -> " . map show
        generateCode' :: (Name, FunctionData) -> String
        generateCode' (n, Sequential args e)
            = defnCode n args ++ show e
        generateCode' (n, Parallel args g)
            = defnCode n args ++ encodeDependencyGraph g True
        generateCode' (n, SequentialT ts args e)
            = typeCode n ts ++ defnCode n args ++ show e
        generateCode' (n, ParallelT ts args g)
            = typeCode n ts ++ defnCode n args ++ encodeDependencyGraph g True


writeCode :: String -> Prog -> Int -> IO ()
writeCode fileName prog steps =
    writeFile fileName $ generateCode $ createFunctionTable prog steps


drawGraphs :: Prog -> IO ()
drawGraphs prog
    = mapM_ (uncurry drawDependencyGraph) ngs
    where 
        ftMap = Map.toList (createFunctionTable prog 0)
        ngs   = [(n ++ ".dot", g) | (n, Parallel _ g) <- ftMap]
