module Parallelizer (
    createFunctionTable,
    FunctionTable,
    FunctionData(..)
) where

import Simple.Syntax

import Prelude hiding (EQ, GT, LT)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import DependencyGraph

type FunctionDefn = Expr
type FunctionCplx = Expr
-- TODO: consider using maybes for defn and cplx
type InitFunctionData = ([Name], FunctionDefn, FunctionCplx)
type InitFunctionTable = Map.Map Name InitFunctionData

type FunctionTable = Map.Map Name FunctionData
data FunctionData
    = Sequential [Name] FunctionDefn
    | Parallel [Name] DependencyGraph
    deriving Show


createFunctionTable :: Prog -> FunctionTable
createFunctionTable = splitFunctions 100 . buildInitFunctionTable


buildInitFunctionTable :: Prog -> InitFunctionTable
buildInitFunctionTable 
    = foldl buildFunctionTable' Map.empty
    where
        buildFunctionTable' :: InitFunctionTable -> Decl -> InitFunctionTable
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
    where bound = Set.fromList $ map (\(Def name _) -> name) defs
freeVariables (App e1 e2)
    = Set.union (freeVariables e1) (freeVariables e2)
freeVariables (Var n)
    = Set.singleton n
freeVariables Lit{}
    = Set.empty
freeVariables (Op _ e1 e2)
    = Set.union (freeVariables e1) (freeVariables e2)


splitFunctions :: Int -> InitFunctionTable -> FunctionTable
splitFunctions steps
    = Map.foldlWithKey splitFunction Map.empty
    where
        splitFunction :: FunctionTable -> Name -> InitFunctionData -> FunctionTable
        splitFunction st name fd @ (args, defn, Lit{})
            = Map.insert name (Sequential args defn) st
        splitFunction st name (args, defn, cplx)
            = Map.union split st
            where
                split          = Map.fromList 
                                    [ (name, Sequential args branchingCall)
                                    , (seqName, Sequential args defn)
                                    , (parName, Parallel args (createDependencyGraph args defn))
                                    ]
                seqName        = name ++ "_seq"
                parName        = name ++ "_par"
                callFunction n = foldl (\app a -> App app (Var a)) (Var n) args 
                branchingCall  = If (Op LT cplx (Lit (LInt steps)))
                                                (callFunction $ name ++ "_seq")
                                                (callFunction $ name ++ "_par")
                                 