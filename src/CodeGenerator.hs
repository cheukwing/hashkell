module CodeGenerator where

import Parallelizer
import Simple.Syntax

import Data.List (intercalate)
import qualified Data.Map.Strict as Map

generateCode :: FunctionTable -> String
generateCode
    = intercalate "\n\n" . map generateCode' . Map.toList
    where
        generateCode' :: (Name, FunctionData) -> String
        generateCode' (n, Sequential args e)
            = unwords (n : args) ++ " = " ++ show e
        generateCode' (n, Parallel args g)
            = unwords (n : args) ++ " = "

writeCode :: Prog -> IO ()
writeCode =
    writeFile "code.hs" . generateCode . toFunctionTable