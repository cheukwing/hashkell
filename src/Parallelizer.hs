module Parallelizer where

import Simple.Syntax

import Prelude hiding (EQ, GT, LT)
import qualified Data.Map.Strict as Map

type FunctionTable = Map.Map Name [(Name, Complexity)]
type ComplexityTable = [(Name, Complexity)]


buildFunctionTable :: Prog -> FunctionTable
buildFunctionTable 
    = foldl buildFunctionTable' Map.empty
    where
        buildFunctionTable' :: FunctionTable -> Decl -> FunctionTable
        buildFunctionTable' ft (Complexity name cplxs)
            = case Map.lookup name ft of    
                Just ct -> Map.insert name (zip (map fst ct) cplxs) ft
                Nothing -> Map.insert name (zip (repeat "?") cplxs) ft
        buildFunctionTable' ft (Func (FuncData name args _))
            = case Map.lookup name ft of    
                Just ct -> Map.insert name (zip args (map snd ct)) ft
                Nothing -> Map.insert name (zip args (repeat None)) ft


-- splitFunction takes a function, and splits it into multiple functions if
-- it has a time complexity annotation.
splitFunction :: FunctionTable -> Int -> FuncData -> [Decl]
splitFunction ft steps func @ (FuncData name args expr) 
    = case Map.lookup name ft of
        Nothing -> [Func func]
        Just ct -> 
            -- do not bother splitting if all annotations are None
            if null significantArgs
                then 
                    [Func func]
                else 
                    [ Func $ FuncData name args (
                        If (splitCondition significantArgs steps)
                           (callFunction (name ++ "_par"))
                           (callFunction (name ++ "_seq"))
                        )
                    , Func $ FuncData (name ++ "_seq") args expr
                    -- TODO: call graph builder
                    , Func $ FuncData (name ++ "_par") args expr
                    ]
            where 
                significantArgs = filter ((/= None) . snd) ct
                callFunction n = foldl (\app arg -> App app (Var arg)) (Var n) args


-- splitCondition takes the parsed complexity annotation of a function and
-- returns the expression corresponding to the condition whereby it should be
-- executed in parallel.
splitCondition :: ComplexityTable -> Int -> Expr
splitCondition ct steps
    = foldl (Op Or) e es 
    where
        (e : es) = map toCondition ct
        toCondition :: (Name, Complexity) -> Expr
        toCondition (m, None)
            = Lit (LBool False)
        toCondition (m, Exponential)
            = Op GT (Var m) (Lit (LInt minM))
            where minM = (ceiling . logBase 2 . fromIntegral) steps
        toCondition (m, Polynomial n)
            = Op GT (Var m) (Lit (LInt minM))
            where minM = ceiling (fromIntegral steps ** (1 / fromIntegral n))