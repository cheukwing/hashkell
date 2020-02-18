{-# LANGUAGE TupleSections #-}

module Parallelizer where

import Simple.Syntax

import Prelude hiding (EQ, GT, LT)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.State.Strict

type FunctionDefn = Expr
type FunctionCplx = Expr
-- TODO: consider using maybes for defn and cplx
type FunctionData = ([Name], FunctionDefn, FunctionCplx)
type FunctionTable = Map.Map Name FunctionData

type DName = String
type DTable = Map.Map DName DNode

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

splitFunctions :: FunctionTable -> FunctionTable
splitFunctions 
    = Map.foldlWithKey splitFunction Map.empty
    where
        splitFunction :: FunctionTable -> Name -> FunctionData -> FunctionTable
        splitFunction st name fd @ (_ , _, Lit{})
            = Map.insert name fd st
        splitFunction st name (args, defn, cplx)
            = st'
            where
                st'            = Map.union (Map.fromList [(name, start), (name ++ "_seq", (args, defn, Lit (LInt 1)))]) st
                callFunction n = foldl (\app a -> App app (Var a)) (Var n) args 
                start          = (args, If (Op LT cplx (Lit (LInt 100)))
                                                (callFunction $ name ++ "_seq")
                                                (callFunction $ name ++ "_par")
                                 , cplx)
            

data DNode
    = DBranch [DName]
    | DArg [DName]
    | DIf [DName] DName DName
    | DDep DExpr [DName]
    deriving (Eq, Show)

data DExpr
    = DApp Name [DExpr]
    | DOp BinOp DExpr DExpr
    | DVar Name
    | DLit Lit
    deriving (Eq, Show)

depName :: Int -> String
depName n = "_x" ++ show n

addDependent :: DNode -> DName -> DNode
addDependent (DBranch deps) dep
    = DBranch (dep : deps)
addDependent (DArg deps) dep
    = DArg (dep : deps)
addDependent (DIf deps thenBranch elseBranch) dep
    = DIf (dep : deps) thenBranch elseBranch
addDependent (DDep exp deps) dep
    = DDep exp (dep : deps)
        
addDependentToNode :: DTable -> DName -> DName -> DTable
addDependentToNode dt name dep
    = Map.insert name (addDependent ((Map.!) dt name) dep) dt


{-
createDTable :: FunctionData -> DTable
createDTable (args, defn, _)
    = snd $ toDTable (Map.fromList initTable) "_" 0 defn
    where
        initTable = ("_", DBranch []) : argsEntries
        argsEntries = map (, DArg []) args
-}


type TableState = (DTable, DName, Int)

addDependentToTable :: DName -> State TableState ()
addDependentToTable parent = do
    (dt, base, i) <- get
    put (Map.insert parent (addDependent ((Map.!) dt parent) (depName i)) dt, base, i)

addDependentToTableWithName :: DName -> DName -> State TableState ()
addDependentToTableWithName parent child = do
    (dt, base, i) <- get
    put (Map.insert parent (addDependent ((Map.!) dt parent) child) dt, base, i)

addNodeToTable :: DNode -> State TableState ()
addNodeToTable node = do
    (dt, base, i) <- get
    put (Map.insert (depName i) node dt, base, i)

addNodeToTableWithName :: DNode -> DName -> State TableState ()
addNodeToTableWithName node name = do
    (dt, base, i) <- get
    put (Map.insert name node dt, base, i)

addBranchToTable :: State TableState DName
addBranchToTable = do
    (dt, base, i) <- get
    let bName = "_" ++ show i
    put (Map.insert bName (DBranch []) dt, base, i + 1)
    return bName

setBase :: DName -> State TableState DName
setBase base = do
    (dt, oldBase, i) <- get
    put (dt, base, i)
    return oldBase

incrementNameCounter :: State TableState DName
incrementNameCounter = do
    (dt, base, i) <- get
    put (dt, base, i + 1)
    return (depName i)

_toDTable :: Expr -> State TableState DName
_toDTable (Lit lit) = do
    (_, base, _) <- get
    addDependentToTable base
    addNodeToTable (DDep (DLit lit) [])
    incrementNameCounter
_toDTable (Var var) = do
    addDependentToTable var
    addNodeToTable (DDep (DVar var) [])
    incrementNameCounter
_toDTable (Op op e1 e2) = do
    left <- _toDTable e1
    right <- _toDTable e2
    addDependentToTable left
    addDependentToTable right
    addNodeToTable (DDep (DOp op (DVar left) (DVar right)) [])
    incrementNameCounter
_toDTable e @ App{} = do
    let
        (appName, args) = appArgs e
        appArgs :: Expr -> (Name, [Expr])  
        appArgs (App (Var name) e)
          = (name, [e])
        appArgs (App e1 e2)
          = (name, e2 : es)
          where (name, es) = appArgs e1
    depNames <- mapM _toDTable args
    mapM_ addDependentToTable depNames
    addNodeToTable (DDep (DApp appName (map DVar depNames)) [])
    incrementNameCounter
_toDTable (Let defs e) = do
    let
        defToTable :: Def -> State TableState DName
        defToTable (Def defName exp) = do
            expName <- _toDTable exp
            addDependentToTableWithName expName defName
            addNodeToTableWithName (DDep (DVar expName) []) defName
            return defName
    defNames <- mapM defToTable defs
    expName <- _toDTable e
    mapM_ addDependentToTable defNames
    addDependentToTable expName
    addNodeToTable (DDep (DVar expName) [])
    incrementNameCounter
_toDTable (If e1 e2 e3) = do
    cond <- _toDTable e1
    thenBranch <- addBranchToTable
    base <- setBase thenBranch
    thenExp <- _toDTable e2
    elseBranch <- addBranchToTable
    _ <- setBase elseBranch
    elseExp <- _toDTable e3
    _ <- setBase base
    addDependentToTable cond
    addNodeToTable (DIf [] thenBranch elseBranch)
    incrementNameCounter


{-
isAtomic :: FunctionTable -> Expr -> Bool
isAtomic _ Lit{} 
    = True
isAtomic ft (Var name) 
    = not $ Map.member name ft
isAtomic ft (Op _ e1 e2)
    = isAtomic ft e1 && isAtomic ft e2
isAtomic ft (If e1 e2 e3)
    = isAtomic ft e1 && isAtomic ft e2 && isAtomic ft e3
isAtomic ft (Let defs e)
    = all (map (\Def _ def -> isAtomic ft def) defs) && isAtomic ft e
isAtomic _ App{}
    = False

toDExpr :: Expr -> DExpr
toDExpr (Var name)
    = DVar name
toDExpr (Lit lit)
    = DLit lit
toDExpr (Op op e1 e2)
    = DOp op (toDExpr e1) (toDExpr e2)
toDExpr (App (Var name) e2)
    = DApp name [toDExpr e2]
toDExpr (App e1 e2)
    = DApp name (toDExpr e2 : es)
    where DApp name es = toDExpr e1

-}