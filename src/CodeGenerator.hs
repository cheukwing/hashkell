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


writeCode :: String -> Prog -> IO ()
writeCode fileName =
    writeFile fileName . generateCode . createFunctionTable


drawGraphs :: Prog -> IO ()
drawGraphs prog 
    = mapM_ (uncurry drawDependencyGraph) ngs
    where 
        ftMap = Map.toList (createFunctionTable prog)
        ngs   = [(n ++ ".dot", g) | (n, Parallel _ g) <- ftMap]
