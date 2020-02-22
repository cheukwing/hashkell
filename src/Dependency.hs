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
    deriving (Eq, Show)

data DExpr
    = DApp Name [DExpr]
    | DOp BinOp DExpr DExpr
    | DVar Name
    | DLit Lit
    deriving (Eq, Show)

toTable :: FunctionData -> Table
toTable (args, defn, _)
    = case xn of
        Left x -> Map.insert "_" (Dep x []) t
        _      -> t
    where
        (xn, fs)  = runState (buildTable defn) initState
        t         = table fs
        initState = FunctionState 
            { table   = Map.fromList [("_", Scope [])]
            , args    = args
            , counter = 0
            , scope   = "_"
            }


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
    let t  = table state
        c  = counter state
        s  = scope state
        as = args state
    if parent `elem` as then 
        put (state {table = Map.insert s (addDependent ((Map.!) t s) (depName c)) t})
    else 
        put (state {table = Map.insert parent (addDependent ((Map.!) t parent) (depName c)) t })


addAsDependentOfWithName :: DName -> DName -> State FunctionState ()
addAsDependentOfWithName parent child = do
    state <- get
    let t  = table state
        s  = scope state
        as = args state
    if parent `elem` as then 
        put (state {table = Map.insert s (addDependent ((Map.!) t s) child) t})
    else 
        put (state {table = Map.insert parent (addDependent ((Map.!) t parent) child) t })


existsDependency :: DName -> DName -> Table -> Bool
existsDependency from to table
    = from == to || getNodeAndCheck from
    where 
        getNodeAndCheck = existsDependency' . (Map.!) table
        existsDependency' :: Node -> Bool
        existsDependency' (Scope ns)
            = to `elem` ns || any getNodeAndCheck ns
        existsDependency' (Dep _ ns)
            = to `elem` ns || any getNodeAndCheck ns
        existsDependency' (Cond _ n1 n2 ns)
            = to `elem` ns || any getNodeAndCheck ns
            || getNodeAndCheck n1
            || getNodeAndCheck n2


addAsDependentOfScope :: State FunctionState ()
addAsDependentOfScope = do
    state <- get
    let t = table state
        n = depName $ counter state
        s = scope state
    if existsDependency s n t then
        return ()
    else
        put (state {table = Map.insert s (addDependent ((Map.!) t s) n) t})


addAsDependentOfScopeWithName :: DName -> State FunctionState ()
addAsDependentOfScopeWithName child = do
    state <- get
    let t = table state
        s = scope state
    if existsDependency s child t then
        return ()
    else
        put (state {table = Map.insert s (addDependent ((Map.!) t s) child) t})


addNode :: Node -> State FunctionState ()
addNode node = do
    state <- get
    let t = table state
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
            let addAsDependentIfName =
                    Either.either
                        return
                        (\n -> do
                            addAsDependentOf n
                            return (DVar n))
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
        addNode (Dep (DApp appName (map (Either.either id DVar) ds)) [])
        mapM_ addAsDependentOf (Either.rights ds)
        Right <$> incrementCounter
buildTable (Let defs e) = do
    let
        defToTable :: Def -> State FunctionState ()
        defToTable (Def defName exp) = do
            xn <- buildTable exp
            let addAsDependentIfName = Either.either 
                    (\x -> addAsDependentOfScopeWithName defName >> return x)
                    (\n -> addAsDependentOfWithName n defName >> return (DVar n))
            v <- addAsDependentIfName xn
            addNodeWithName (Dep v []) defName
    mapM_ defToTable defs
    xn <- buildTable e
    Either.either
        (return . Left)
        (\n -> do
            addAsDependentOf n
            addNode (Dep (DVar n) [])
            Right <$> incrementCounter)
        xn
buildTable (If e1 e2 e3) = do
    let 
        setScopeAsParent = Either.either handleExpr addAsDependentOfScopeWithName
        handleExpr e = addAsDependentOfScope >> addNode (Dep e []) >> incrementCounter >> return ()
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
    Either.either
        (\x -> do 
            addNode (Cond x thenScope elseScope [])
            Right <$> incrementCounter)
        (\n -> do 
            addAsDependentOf n 
            addNode (Cond (DVar n) thenScope elseScope []) 
            Right <$> incrementCounter)
        cxn

