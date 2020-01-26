module Parallelizer where

import Simple.Syntax

import Prelude hiding (EQ, GT, LT)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type FunctionDefn = Expr
type FunctionCplx = Expr
type FunctionData = ([Name], FunctionDefn, FunctionCplx)
type FunctionTable = Map.Map Name FunctionData


buildFunctionTable :: Prog -> FunctionTable
buildFunctionTable 
    = foldl buildFunctionTable' Map.empty
    where
        buildFunctionTable' :: FunctionTable -> Decl -> FunctionTable
        buildFunctionTable' ft (Func name args expr)
            | isMember && fvsValid  = Map.insert name (args, expr, cplx) ft
            | otherwise             = Map.insert name (args, expr, Lit (LInt 1)) ft
            where
                isMember     = Map.member name ft
                (_, _, cplx) = (Map.!) ft name
                fvsValid     = Set.foldl (\t v -> ((&&) t . flip elem args) v) True (freeVariables cplx)
        buildFunctionTable' ft (Cplx name expr)
            | isValid && isMember && fvsValid = Map.insert name (args, defn, expr) ft
            | isValid && not isMember         = Map.insert name ([], Lit (LInt 1), expr) ft
            | otherwise                       = ft
            where
                isValid         = isValidComplexity expr
                isMember        = Map.member name ft
                (args, defn, _) = (Map.!) ft name
                fvsValid        = Set.foldl (\t v -> ((&&) t . flip elem args) v) True (freeVariables expr)


isValidComplexity :: Expr -> Bool
isValidComplexity If{}           = False
isValidComplexity Let{}          = False
-- TODO: support log application
isValidComplexity App{}          = False
isValidComplexity Var{}          = True
isValidComplexity (Lit (LInt _)) = True
isValidComplexity Lit{}          = False
isValidComplexity (Op op e1 e2)
    = op `elem` [Add, Sub, Mul, Div, Exp] 
        && isValidComplexity e1 
        && isValidComplexity e2


freeVariables :: Expr -> Set.Set Name
freeVariables (If e1 e2 e3)
    = Set.unions [freeVariables e1, freeVariables e2, freeVariables e3]
freeVariables (Let defs e)
    = Set.difference (freeVariables e) bound
    where
        bound = Set.fromList $ map (\(Def name _) -> name) defs
freeVariables (App e1 e2)
    = Set.union (freeVariables e1) (freeVariables e2)
freeVariables (Var n)
    = Set.singleton n
freeVariables Lit{}
    = Set.empty
freeVariables (Op _ e1 e2)
    = Set.union (freeVariables e1) (freeVariables e2)

-- splitFunction takes a function, and splits it into multiple functions if
-- it has a time complexity annotation.
-- splitFunction :: FunctionTable -> Int -> FuncData -> [Decl]
-- splitFunction ft steps func @ (FuncData name args expr) 
--     = case Map.lookup name ft of
--         Nothing -> [Func func]
--         Just ct -> 
--             -- do not bother splitting if all annotations are None
--             if null significantArgs
--                 then 
--                     [Func func]
--                 else 
--                     [ Func $ FuncData name args (
--                         If ((complexityCondition steps . head) significantArgs)
--                            (callFunction (name ++ "_par"))
--                            (callFunction (name ++ "_seq"))
--                         )
--                     , Func $ FuncData (name ++ "_seq") args expr
--                     -- TODO: call graph builder
--                     , Func $ FuncData (name ++ "_par") args expr
--                     ]
--             where 
--                 significantArgs = filter ((/= None) . snd) ct
--                 callFunction n = foldl (\app arg -> App app (Var arg)) (Var n) args