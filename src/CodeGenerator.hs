module CodeGenerator where

import Parallelizer
import Simple.Syntax
import DependencyGraph

import Data.List (intercalate)
import qualified Data.Map.Strict as Map

generateCode :: FunctionTable -> String
generateCode
    = intercalate "\n\n" . map generateCode' . Map.toList
    where
        defnCode n args = unwords (n : args) ++ " = "
        generateCode' :: (Name, FunctionData) -> String
        generateCode' (n, Sequential args e)
            = defnCode n args ++ show e
        generateCode' (n, Parallel args g)
            = defnCode n args ++ encodeDependencyGraph g True


writeCode :: String -> Prog -> Int -> IO ()
writeCode fileName prog steps =
    writeFile fileName $ generateCode $ createFunctionTable prog steps


drawGraphs :: Prog -> Int -> IO ()
drawGraphs prog steps
    = mapM_ (uncurry drawDependencyGraph) ngs
    where 
        ftMap = Map.toList (createFunctionTable prog steps)
        ngs   = [(n ++ ".dot", g) | (n, Parallel _ g) <- ftMap]
