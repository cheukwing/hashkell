{-# LANGUAGE TupleSections #-}

module Dependency where

import Simple.Syntax
import Parallelizer

import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict

type DName = String
type DTable = Map.Map DName DNode

data DNode
    = DBranch [DName]
    | DArg [DName]
    | DIf DName DName [DName] 
    | DDep DExpr [DName]
    deriving (Eq, Show)

data DExpr
    = DApp Name [DExpr]
    | DOp BinOp DExpr DExpr
    | DVar Name
    | DLit Lit
    deriving (Eq, Show)

type TableState = (DTable, DName, Int)

depName :: Int -> String
depName = (++) "_x" . show

branchName :: Int -> String
branchName =  (++) "_" . show

addDependent :: DNode -> DName -> DNode
addDependent (DBranch deps) dep
    = DBranch (dep : deps)
addDependent (DArg deps) dep
    = DArg (dep : deps)
addDependent (DIf thenBranch elseBranch deps) dep
    = DIf thenBranch elseBranch (dep : deps)
addDependent (DDep exp deps) dep
    = DDep exp (dep : deps)

        
addDependentToNode :: DTable -> DName -> DName -> DTable
addDependentToNode dt name dep
    = Map.insert name (addDependent ((Map.!) dt name) dep) dt


createDTable :: FunctionData -> DTable
createDTable (args, defn, _)
    = table
    where
        (table, _, _) = execState (toDTable defn) initState
        initState = (Map.fromList initTable, "_", 0)
        initTable = ("_", DBranch []) : argsEntries
        argsEntries = map (, DArg []) args


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
    put (Map.insert (branchName i) (DBranch []) dt, base, i + 1)
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


toDTable :: Expr -> State TableState DName
toDTable (Lit lit) = do
    (_, base, _) <- get
    addDependentToTable base
    addNodeToTable (DDep (DLit lit) [])
    incrementNameCounter
toDTable (Var var) = do
    addDependentToTable var
    addNodeToTable (DDep (DVar var) [])
    incrementNameCounter
toDTable (Op op e1 e2) = do
    left <- toDTable e1
    right <- toDTable e2
    addDependentToTable left
    addDependentToTable right
    addNodeToTable (DDep (DOp op (DVar left) (DVar right)) [])
    incrementNameCounter
toDTable e @ App{} = do
    let
        (appName, args) = appArgs e
        appArgs :: Expr -> (Name, [Expr])  
        appArgs (App (Var name) e)
          = (name, [e])
        appArgs (App e1 e2)
          = (name, e2 : es)
          where (name, es) = appArgs e1
    depNames <- mapM toDTable args
    mapM_ addDependentToTable depNames
    addNodeToTable (DDep (DApp appName (map DVar depNames)) [])
    incrementNameCounter
toDTable (Let defs e) = do
    let
        defToTable :: Def -> State TableState DName
        defToTable (Def defName exp) = do
            expName <- toDTable exp
            addDependentToTableWithName expName defName
            addNodeToTableWithName (DDep (DVar expName) []) defName
            return defName
    defNames <- mapM defToTable defs
    expName <- toDTable e
    mapM_ addDependentToTable defNames
    addDependentToTable expName
    addNodeToTable (DDep (DVar expName) [])
    incrementNameCounter
toDTable (If e1 e2 e3) = do
    cond <- toDTable e1
    thenBranch <- addBranchToTable
    base <- setBase thenBranch
    thenExp <- toDTable e2
    elseBranch <- addBranchToTable
    _ <- setBase elseBranch
    elseExp <- toDTable e3
    _ <- setBase base
    addDependentToTable cond
    addNodeToTable (DIf thenBranch elseBranch [])
    incrementNameCounter