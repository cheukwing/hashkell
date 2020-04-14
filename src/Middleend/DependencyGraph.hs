module Middleend.DependencyGraph where

import Simple.Syntax

import Control.Monad.State.Strict
import Control.Monad (void)
import Data.Either (Either(..), either)
import qualified Data.Either as Either
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (Maybe)
import qualified Data.Maybe as Maybe

type DependencyGraph = (NodeTable, Dependencies)
type NodeTable = Map Name DNode
type Dependencies = Set Dependency
type Dependency = (Name, Name, DType)

data DType
    = Dep
    | DepThen
    | DepElse
    | DepParam
    deriving (Eq, Show, Ord)

data DNode
    = Scope
    | Expression DExpr
    | Conditional DExpr
    deriving (Eq, Show)

data DExpr
    = DApp Name [DExpr]
    | DOp BinOp DExpr DExpr
    | DVar Name
    | DLit DLit
    deriving (Eq, Show)

data DLit
    = DInt Int
    | DBool Bool
    | DList [DExpr]
    deriving (Eq, Show)


type Params = [Name]
type Counter = Int
type CurrentScope = Name
type FunctionState = (DependencyGraph, Params, Counter, CurrentScope)

createDependencyGraph :: [Name] -> Expr -> DependencyGraph
createDependencyGraph params defn
    = dg
    where
        initState     = ( (Map.fromList [("_", Scope)], Set.empty)
                        , params
                        , 0
                        , "_"
                        )
        (dg, _, _, _) = execState (buildGraph defn) initState


-- depName returns a unique identifier based on the current counter.
depName :: State FunctionState Name
depName = do
    (_, _, counter, _) <- get
    return ("_x" ++ show counter)


-- scopeName returns an unique name for the current scope based on the current
-- counter. 
scopeName :: State FunctionState Name
scopeName = do
    (_, _, counter, _) <- get
    return ("_" ++ show counter)


-- setScope sets the current scope to the given scope, and returns the previous
-- scope in the state.
setScope :: Name -> State FunctionState Name
setScope s = do
    (graph, params, counter, scope) <- get
    put (graph, params, counter, s)
    return scope


-- hasParent checks if the node with the given name has any parent nodes.
hasParent :: DependencyGraph -> Name -> Bool
hasParent (_, ds) name =
    not $ null $ Set.filter (\(_, c, t) -> c == name && t == Dep) ds


-- addArc inserts a dependency arc into the state
addArc :: Dependency -> State FunctionState ()
addArc d = do
    ((ns, ds), params, counter, scope) <- get
    put ((ns, Set.insert d ds), params, counter, scope)


-- isDescendentOf checks if a is a descendent of b by recursively checking
-- a's parents for b
isDescendentOf :: Name -> Name -> State FunctionState Bool
a `isDescendentOf` b = do
    ((_, ds), _, _, _) <- get
    let 
        pes = Set.filter (\(_, c, t) -> c == a && t == Dep) ds
        ps  = Set.toList $ Set.map (\(p, _, _) -> p) pes
    parentsAreDescendent <- or <$> mapM (`isDescendentOf` b) ps
    -- b being an element of ps is handled in the next call
    return (a == b || parentsAreDescendent)


-- addDependency adds an arc from parent to child into the state
-- TODO: If a->b and c = a + b, then we should only have one dependency
-- from b->c, instead of a->c and b->c
addDependency :: Name -> Name -> State FunctionState ()
addDependency child parent = do
    (graph, params, counter, scope) <- get
    if parent `elem` params then do
        -- setup an arc to signify a use of a param in this node
        addArc ("_", child, DepParam)
        depAlreadyExists <- child `isDescendentOf` scope
        if depAlreadyExists
            then return ()
            else addArc (scope, child, Dep)
    else do
        depAlreadyExists <- child `isDescendentOf` parent
        if depAlreadyExists
            then return ()
            else addArc (parent, child, Dep)


-- addScopeDependency explicitly adds an arc from the current scope to the 
-- child, if such an arc does not already exist
addScopeDependency :: Name -> State FunctionState ()
addScopeDependency child = do
    (graph, params, counter, scope) <- get
    if hasParent graph child 
        then return ()
        else addArc (scope, child, Dep)


-- addConditionalDependency sets up the dependency arcs for a conditional node,
-- given its 'then' and 'else' scope nodes.
addConditionalDependency :: Name -> Name -> Name -> State FunctionState ()
addConditionalDependency c t e = do
    addArc (c, e, DepElse)
    addArc (c, t, DepThen)


-- addDependencyNode inserts a node with the given name into the state.
addDependencyNode :: Name -> DNode -> State FunctionState ()
addDependencyNode name node = do
    ((ns, ds), params, counter, scope) <- get
    put ((Map.insert name node ns, ds), params, counter, scope)


-- addScope inserts a scope node into the state using the current counter,
-- then returns the generated name.
addScope :: State FunctionState Name
addScope = do
    name <- scopeName
    addDependencyNode name Scope
    return name


-- incrementCounter increments the current counter, while also returning the
-- depName generated from the previous counter for reference.
incrementCounter :: State FunctionState Name
incrementCounter = do
    (graph, params, counter, scope) <- get
    name <- depName
    put (graph, params, counter + 1, scope)
    return name


atomicToDExpr :: Expr -> Maybe DExpr
atomicToDExpr (Lit (LInt i))
    = return $ DLit (DInt i)
atomicToDExpr (Lit (LBool b))
    = return $ DLit (DBool b)
atomicToDExpr (Lit (LList ls)) = do
    let ls' = Maybe.mapMaybe atomicToDExpr ls
    if length ls' /= length ls
        then Nothing
        else return $ DLit (DList ls')
atomicToDExpr (Var n)
    = return $ DVar n
atomicToDExpr (Op op e1 e2) = do
    e1' <- atomicToDExpr e1
    e2' <- atomicToDExpr e2
    return $ DOp op e1' e2'
atomicToDExpr _
    = Nothing
    
dependenciesFromDExpr :: DExpr -> [Name]
dependenciesFromDExpr (DLit (DList es))
    = concatMap dependenciesFromDExpr es
dependenciesFromDExpr DLit{}
    = []
dependenciesFromDExpr (DVar n)
    = [n]
dependenciesFromDExpr (DOp _ e1 e2)
    = dependenciesFromDExpr e1 ++ dependenciesFromDExpr e2
dependenciesFromDExpr (DApp _ es)
    = concatMap dependenciesFromDExpr es


setupDependencies :: Name -> DExpr -> State FunctionState ()
setupDependencies name dexpr = do
    let parents = dependenciesFromDExpr dexpr
    if null parents
        then void $ addScopeDependency name
        else mapM_ (addDependency name) parents

setupExpressionNode :: Name -> DExpr -> State FunctionState ()
setupExpressionNode name dexpr = do
    addDependencyNode name (Expression dexpr)
    setupDependencies name dexpr


-- buildExpr converts atomics to expressions, builds nodes for non-atomics
buildExpr :: Expr -> State FunctionState DExpr
buildExpr e
    = Maybe.maybe (DVar <$> buildGraph e)
        return
        (atomicToDExpr e)

buildGraph :: Expr -> State FunctionState Name
buildGraph (Lit lit) = do
    name <- incrementCounter
    dexpr <- DLit <$> (case lit of
        LInt i  -> return $ DInt i
        LBool b -> return $ DBool b
        LList ls -> DList <$> mapM buildExpr ls)
    setupExpressionNode name dexpr
    return name
buildGraph (Var var) = do
    name <- incrementCounter
    setupExpressionNode name (DVar var)
    return name
buildGraph (Op op e1 e2) = do
    name <- incrementCounter
    dexpr <- DOp op <$> buildExpr e1 <*> buildExpr e2
    setupExpressionNode name dexpr
    return name
buildGraph e @ App{} = do
    name <- incrementCounter
    let (fName, args) = collectApp e
        collectApp :: Expr -> (Name, [Expr])
        collectApp (App (Var name) e)
            = (name, [e])
        collectApp (App e1 e2)
            = (name, es ++ [e2])
            where (name, es) = collectApp e1
    dexpr <- DApp fName <$> mapM buildExpr args
    setupExpressionNode name dexpr
    return name
buildGraph (Let defs e) = do
    name <- incrementCounter
    let buildDef (Def defName defE) =
            buildExpr defE >>= setupExpressionNode defName
    mapM_ buildDef defs
    dexpr <- buildExpr e
    setupExpressionNode name dexpr
    return name
buildGraph (If e1 e2 e3) = do
    name <- incrementCounter
    let buildBranch e = do
            oldScope <- addScope >>= setScope
            incrementCounter
            buildGraph e
            setScope oldScope
    condDExpr <- buildExpr e1
    addDependencyNode name (Conditional condDExpr)
    setupDependencies name condDExpr
    thenScope <- buildBranch e2
    elseScope <- buildBranch e3
    addConditionalDependency name thenScope elseScope
    return name