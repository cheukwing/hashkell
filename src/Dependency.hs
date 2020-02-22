{-# LANGUAGE TupleSections #-}

module Dependency where

import Simple.Syntax
import Parallelizer

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Either (Either(..))
import qualified Data.Either as Either
import Control.Monad.State.Strict

type DName = String
type Table = Map DName Node
data FunctionState = FunctionState
    { table :: Table
    , args :: [Name]
    , counter :: Int
    , scope :: DName
    }

data Node
    = Scope [DName]
    | Dep DExpr [DName]
    | Cond DExpr DName DName [DName]

data DExpr
    = DApp Name [DExpr]
    | DOp BinOp DExpr DExpr
    | DVar Name
    | DLit Lit
    deriving (Eq, Show)

{-
createPTable :: FunctionData -> PTable
createPTable (args, defn, _)
    = table
    where
        (table, _, _) = execState (toPTable defn) initState
        initState = (Map.fromList initTable, "_", 0)
        initTable = ("_", PBranch []) : argsEntries
        argsEntries = map (, PArg []) args
-}

depName :: Int -> String
depName = (++) "_x" . show


scopeName :: Int -> String
scopeName =  (++) "_" . show


setScope :: DName -> State FunctionState DName
setScope s = do
    state <- get
    let oldS = scope state
    put (state {scope = s})
    return oldS


addDependent :: Node -> DName -> Node
addDependent node name
    = case node of
        Scope names        -> Scope (addIfNotPresent name names)
        Dep e names        -> Dep e (addIfNotPresent name names)
        Cond e n1 n2 names -> Cond e n1 n2 (addIfNotPresent name names)
    where addIfNotPresent n ns = if n `elem` ns then ns else n : ns


addAsDependentOf :: DName -> State FunctionState ()
addAsDependentOf parent = do
    state <- get
    let
        t = table state
        c = counter state
        as = args state
    if parent `elem` as then 
        put (state {table = Map.insert "_" (addDependent ((Map.!) t "_") (depName c)) t})
    else 
        put (state {table = Map.insert parent (addDependent ((Map.!) t parent) (depName c)) t })

addAsDependentOfWithName :: DName -> DName -> State FunctionState ()
addAsDependentOfWithName parent child = do
    state <- get
    let
        t = table state
        as = args state
    if parent `elem` as then 
        put (state {table = Map.insert "_" (addDependent ((Map.!) t "_") child) t})
    else 
        put (state {table = Map.insert parent (addDependent ((Map.!) t parent) child) t })

addAsDependentOfScope :: State FunctionState ()
addAsDependentOfScope = do
    state <- get
    let
        t = table state
        c = counter state
        s = scope state
    put (state {table = Map.insert s (addDependent ((Map.!) t s) (depName c)) t})

addAsDependentOfScopeWithName :: DName -> State FunctionState ()
addAsDependentOfScopeWithName child = do
    state <- get
    let
        t = table state
        s = scope state
    put (state {table = Map.insert s (addDependent ((Map.!) t s) child) t})


addNode :: Node -> State FunctionState ()
addNode node = do
    state <- get
    let
        t = table state
        c = counter state
    put (state {table = Map.insert (depName c) node t})

addNodeWithName :: Node -> DName -> State FunctionState ()
addNodeWithName node name = do
    state <- get
    let t = table state
    put (state {table = Map.insert name node t})


addScope :: State FunctionState DName
addScope = do
    state <- get
    let
        t = table state
        c = counter state
    put (state {table = Map.insert (scopeName c) (Scope []) t, counter = c + 1})
    return (scopeName c)


incrementCounter :: State FunctionState DName
incrementCounter = do
    state <- get
    let c = counter state
    put (state {counter = c + 1})
    return (depName c)


buildTable :: Expr -> State FunctionState (Either DExpr DName)
buildTable (Lit lit) =
    return (Left (DLit lit))
buildTable (Var var) =
    return (Right var)
buildTable (Op op e1 e2) = do
    xn1 <- buildTable e1
    xn2 <- buildTable e2
    case (xn1, xn2) of
        (Left x1, Left x2) ->
            return $ Left (DOp op x1 x2)
        _                  -> do
            let addAsDependentIfName xn =
                    case xn of
                        Left x -> 
                            return x
                        Right n -> do
                            addAsDependentOf n
                            return (DVar n)
            v1 <- addAsDependentIfName xn1
            v2 <- addAsDependentIfName xn2
            addNode (Dep (DOp op v1 v2) [])
            Right <$> incrementCounter
buildTable e @ App{} = do
    let
        (appName, args) = appArgs e
        appArgs :: Expr -> (Name, [Expr])  
        appArgs (App (Var name) e)
          = (name, [e])
        appArgs (App e1 e2)
          = (name, e2 : es)
          where (name, es) = appArgs e1
    ds <- mapM buildTable args
    if all Either.isLeft ds then do
        addAsDependentOfScope
        addNode (Dep (DApp appName (Either.lefts ds)) [])
        Right <$> incrementCounter
    else do
        mapM_ addAsDependentOf (Either.rights ds)
        addNode (Dep (DApp appName (map (Either.either id DVar) ds)) [])
        Right <$> incrementCounter
buildTable (Let defs e) = do
    let
        defToTable :: Def -> State FunctionState ()
        defToTable (Def defName exp) = do
            xn <- buildTable exp
            let
                addAsDependentIfName :: Either DExpr DName -> State FunctionState DExpr 
                addAsDependentIfName xn =
                    case xn of
                        Left x -> do
                            addAsDependentOfScopeWithName defName
                            return x
                        Right n -> do
                            addAsDependentOfWithName n defName
                            return (DVar n)
            v <- addAsDependentIfName xn
            addNodeWithName (Dep v []) defName
    mapM_ defToTable defs
    xn <- buildTable e
    case xn of
        Left x ->
            return (Left x)
        Right n -> do
            addAsDependentOf n
            addNode (Dep (DVar n) [])
            Right <$> incrementCounter
buildTable (If e1 e2 e3) = do
    let
        setScopeAsParent :: Either DExpr DName -> State FunctionState ()
        setScopeAsParent
            = Either.either handleExpr addAsDependentOfScopeWithName
            where
                handleExpr e = do
                    addNode (Dep e [])
                    incrementCounter
                    return ()
    cxn <- buildTable e1
    thenScope <- addScope
    s <- setScope thenScope
    txn <- buildTable e2
    setScopeAsParent txn
    elseScope <- addScope
    setScope elseScope
    exn <- buildTable e3
    setScopeAsParent exn
    setScope s
    case cxn of
        Left x -> do
            addNode (Cond x thenScope elseScope [])
            Right <$> incrementCounter
        Right n -> do
            addAsDependentOf n
            addNode (Cond (DVar n) thenScope elseScope [])
            Right <$> incrementCounter

