module Parallelizer where

import Simple.Syntax

import Prelude hiding (EQ, GT, LT)
import qualified Data.Map.Strict as Map

type FunctionTable = Map.Map Name ComplexityTable

type ComplexityTable = Map.Map Name Complexity

-- type FunctionTable = Map Name [(Name, Complexity)]

-- buildFunctionTable :: Prog -> FunctionTable
-- buildFunctionTable 
--     = foldl buildFunctionTable' Map.empty
--     where
--         buildFunctionTable' :: FunctionTable -> Decl -> FunctionTable
--         buildFunctionTable ft (Complexity name cplx)
--             = 

splitFunction :: FunctionTable -> FuncData -> [Decl]
splitFunction ft func @ (FuncData name args expr)
    = case Map.lookup name ft of
        Nothing -> [Func func]
        Just cplxs -> 
            if Map.null significantArgs
                then [Func func]
                else 
                    [ Func $ FuncData name args (
                        If (splitCondition significantArgs)
                           (callFunction (name ++ "_par"))
                           (callFunction (name ++ "_seq"))
                        )
                    , Func $ FuncData (name ++ "_seq") args expr
                    -- TODO: call graph builder
                    , Func $ FuncData (name ++ "_par") args expr
                    ]
            where 
                significantArgs = Map.filter (/= None) cplxs
                callFunction n = foldl (\app arg -> App app (Var arg)) (Var n) args

splitCondition :: ComplexityTable -> Expr
splitCondition ct
    = foldl (Op And) c cs 
    where
        (c : cs) = Map.elems $ Map.mapWithKey toCondition ct
        toCondition :: Name -> Complexity -> Expr
        -- Pre: no 'None' complexity
        toCondition m None 
            = error "cannot convert None complexity to condition"
        toCondition m Factorial
            = Op GT (Var m) (Lit (LInt 5))
        toCondition m (Polynomial n)
            = Op GT (head $ take n $ iterate (\mult -> Op Mul mult (Var m)) (Var m)) (Lit (LInt 100))