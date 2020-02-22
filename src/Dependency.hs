{-# LANGUAGE TupleSections #-}

module Dependency where

import Simple.Syntax
import Parallelizer

import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict

type DName = String
type PTable = Map.Map DName PNode
type TableState = (PTable, DName, Int)

data DNode
    = DNone [DName]
    | DBranch [DName] [DName] [DName]
    | DDep DName [DNode]

data PNode
    = PBranch [DName]
    | PArg [DName]
    | PIf DExpr DName DName [DName] 
    | PDep DExpr [DName]
    deriving (Eq, Show)

data DExpr
    = DApp Name [DExpr]
    | DOp BinOp DExpr DExpr
    | DVar Name
    | DLit Lit
    deriving (Eq, Show)

createPTable :: FunctionData -> PTable
createPTable (args, defn, _)
    = table
    where
        (table, _, _) = execState (toPTable defn) initState
        initState = (Map.fromList initTable, "_", 0)
        initTable = ("_", PBranch []) : argsEntries
        argsEntries = map (, PArg []) args


depName :: Int -> String
depName = (++) "_x" . show

branchName :: Int -> String
branchName =  (++) "_" . show

addDependent :: PNode -> DName -> PNode
addDependent (PBranch deps) dep
    = PBranch (dep : deps)
addDependent (PArg deps) dep
    = PArg (dep : deps)
addDependent (PIf exp thenBranch elseBranch deps) dep
    = PIf exp thenBranch elseBranch (dep : deps)
addDependent (PDep exp deps) dep
    = PDep exp (dep : deps)


addDependentToTable :: DName -> State TableState ()
addDependentToTable parent = do
    (dt, base, i) <- get
    put (Map.insert parent (addDependent ((Map.!) dt parent) (depName i)) dt, base, i)


addDependentToTableWithName :: DName -> DName -> State TableState ()
addDependentToTableWithName parent child = do
    (dt, base, i) <- get
    put (Map.insert parent (addDependent ((Map.!) dt parent) child) dt, base, i)


addNodeToTable :: PNode -> State TableState ()
addNodeToTable node = do
    (dt, base, i) <- get
    put (Map.insert (depName i) node dt, base, i)


addNodeToTableWithName :: PNode -> DName -> State TableState ()
addNodeToTableWithName node name = do
    (dt, base, i) <- get
    put (Map.insert name node dt, base, i)


addBranchToTable :: State TableState DName
addBranchToTable = do
    (dt, base, i) <- get
    put (Map.insert (branchName i) (PBranch []) dt, base, i + 1)
    return (branchName i)


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


toPTable :: Expr -> State TableState DName
toPTable (Lit lit) = do
    (_, base, _) <- get
    addDependentToTable base
    addNodeToTable (PDep (DLit lit) [])
    incrementNameCounter
-- TODO: fix scoping of vars
toPTable (Var var) =
    return var
toPTable (Op op e1 e2) = do
    left <- toPTable e1
    right <- toPTable e2
    addDependentToTable left
    addDependentToTable right
    addNodeToTable (PDep (DOp op (DVar left) (DVar right)) [])
    incrementNameCounter
toPTable e @ App{} = do
    let
        (appName, args) = appArgs e
        appArgs :: Expr -> (Name, [Expr])  
        appArgs (App (Var name) e)
          = (name, [e])
        appArgs (App e1 e2)
          = (name, e2 : es)
          where (name, es) = appArgs e1
    depNames <- mapM toPTable args
    mapM_ addDependentToTable depNames
    addNodeToTable (PDep (DApp appName (map DVar depNames)) [])
    incrementNameCounter
-- TODO: fix scoping of definitions
toPTable (Let defs e) = do
    let
        defToTable :: Def -> State TableState DName
        defToTable (Def defName exp) = do
            expName <- toPTable exp
            addDependentToTableWithName expName defName
            addNodeToTableWithName (PDep (DVar expName) []) defName
            return defName
    defNames <- mapM defToTable defs
    expName <- toPTable e
    mapM_ addDependentToTable defNames
    addDependentToTable expName
    addNodeToTable (PDep (DVar expName) [])
    incrementNameCounter
toPTable (If e1 e2 e3) = do
    cond <- toPTable e1
    thenBranch <- addBranchToTable
    base <- setBase thenBranch
    thenExp <- toPTable e2
    elseBranch <- addBranchToTable
    _ <- setBase elseBranch
    elseExp <- toPTable e3
    _ <- setBase base
    addDependentToTable cond
    addNodeToTable (PIf (DVar cond) thenBranch elseBranch [])
    incrementNameCounter