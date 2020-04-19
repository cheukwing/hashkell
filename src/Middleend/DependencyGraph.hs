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

import Debug.Trace (trace)

type DependencyGraph = (NodeTable, Dependencies)
type NodeTable = Map Name DNode
type Dependencies = Set Dependency
type Dependency = (Name, Name, DType)

data DType
    = Dep
    | DepThen
    | DepElse
--    | DepParam
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

-- createDependencyGraph builds the dependency graph of a function with the
-- given params and definition
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


-- setScope sets the current scope to the given scope, and returns the previous
-- scope in the state.
setScope :: Name -> State FunctionState Name
setScope s = do
    (graph, params, counter, scope) <- get
    put (graph, params, counter, s)
    return scope


-- hasParent checks if the node with the given name has any parent nodes.
hasParent :: Name -> State FunctionState Bool
hasParent name = do
    ((_, ds), _, _, scope) <- get
    let dependencies = Set.filter (\(_, c, t) -> c == name && t == Dep) ds
    scopedPs <- filterM (\(p, _, _) -> p `isDescendentOf` scope) 
        (Set.toList dependencies)
    return (not (null scopedPs))


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

-- removeSimilarDependencies removes any dependency arcs which will be covered
-- transitively if we were to add the given arc
-- does not remove that arc (if it exists), but will not be a problem since
-- the dependencies are stored in a Set
removeSimilarDependencies :: Name -> Name -> State FunctionState ()
removeSimilarDependencies child parent = do
    ((ns, ds), params, counter, scope) <- get
    let similars = Set.toList $ Set.filter 
            (\(p, c, _) -> c == child && p /= parent)
            ds
    similars' <- Set.fromList 
        <$> filterM (\(p, _, _) -> parent `isDescendentOf` p) similars
    put ((ns, Set.difference ds similars'), params, counter, scope)


-- addDependency adds an arc from parent to child into the state
addDependency :: Name -> Name -> State FunctionState ()
addDependency child parent = do
    (graph, params, counter, scope) <- get
--        addArc ("_", child, DepParam)
    let p = if parent `elem` params then scope else parent
    -- check if we already have the dependency, or it exists transitively
    depAlreadyExists <- child `isDescendentOf` p
    unless depAlreadyExists $ do
        -- if it does not exist transitively, add the arc then remove any
        -- dependencies which are now redundandant through transitivity
        addArc (p, child, Dep)
        removeSimilarDependencies child p


-- addScopeDependency explicitly adds an arc from the current scope to the 
-- child, if the child is not already a descendent of the scope
addScopeDependency :: Name -> State FunctionState ()
addScopeDependency child = do
    (graph, params, counter, scope) <- get
    alreadyDescendent <- child `isDescendentOf` scope
    unless alreadyDescendent $ addArc (scope, child, Dep)


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
-- increments the counter, then returns the generated name.
addScope :: State FunctionState Name
addScope = do
    (graph, params, counter, scope) <- get
    let name = "_" ++ show counter
    put (graph, params, counter + 1, scope)
    addDependencyNode name Scope
    return name


-- depName returns a new generated identifier name using the current counter
-- value, then increments it for next use
depName :: State FunctionState Name
depName = do
    (graph, params, counter, scope) <- get
    let name = "_x" ++ show counter
    put (graph, params, counter + 1, scope)
    return name


-- atomicToDExpr returns a DExpr if the expression is atomic, else Nothing
-- atomic function calls are handled separately in buildExpr because they may
-- contain non-atomic arguments which need nodes to be built
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
    
-- dependenciesFromDExpr returns all the names of the dependencies of the
-- given DExpr
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

-- setupDependencies adds all the dependencies for a node with the given name
-- and DExpr
setupDependencies :: Name -> DExpr -> State FunctionState ()
setupDependencies name dexpr = do
    let parents = dependenciesFromDExpr dexpr
    if null parents
        -- add a scope dependency to ensure that they stay within their scope
        then addScopeDependency name
        else mapM_ (addDependency name) parents >> addScopeDependency name

-- setupExpressionNode setups up the creation of a node and its dependencies
setupExpressionNode :: Name -> DExpr -> State FunctionState ()
setupExpressionNode name dexpr = do
    addDependencyNode name (Expression dexpr)
    setupDependencies name dexpr


-- collectApp returns the name of the function being called, and all the
-- expressions being passed as arguments from an App expression
collectApp :: Expr -> (Name, [Expr])
collectApp (App (Var name) e)
    = (name, [e])
collectApp (App e1 e2)
    = (name, es ++ [e2])
    where (name, es) = collectApp e1


-- buildOpExpr combines several operations expressions so that they will
-- appear as a single node, avoiding nodes for each bracketing
-- as in e.g. ((x + y) + z) + w causing 3 nodes
buildOpExpr :: Expr -> State FunctionState DExpr
buildOpExpr (Op op e1 e2)
    = case (atomicToDExpr e1, atomicToDExpr e2) of
        (Nothing, Nothing)   -> DOp op <$> buildOpExpr e1 <*> buildOpExpr e2
        (Just de1, Nothing)  -> DOp op de1 <$> buildOpExpr e2
        (Nothing, Just de2)  -> DOp op <$> buildOpExpr e1 <*> return de2
        (Just de1, Just de2) -> return $ DOp op de1 de2
buildOpExpr e
    = buildExpr e

-- isAtomicFunction returns True if the given name is the name of an atomic
-- function, i.e. a O(1) in-built function
-- NOTE: incomplete list of functions
isAtomicFunction :: Name -> Bool
isAtomicFunction
    = flip elem ["null", "head", "tail"]

-- buildExpr converts atomics to expressions, builds nodes for non-atomics
buildExpr :: Expr -> State FunctionState DExpr
buildExpr e
    = case (atomicToDExpr e, e) of
        -- if atomic, then can just return dexpr for embedding
        (Just de, _)    -> return de
        (_, e' @ App{}) -> do
            let (fName, args) = collectApp e
            if isAtomicFunction fName
                -- if the function is atomic, then can just return dexpr
                -- for embedding
                then DApp fName <$> mapM buildExpr args
                -- otherwise, build the node as usual
                else buildNode
        -- if not atomic, build the node
        _               -> buildNode
    where buildNode = DVar <$> buildGraph e


-- buildGraph builds a node for the given expression with a unique
-- generated name
buildGraph :: Expr -> State FunctionState Name
buildGraph = buildGraphWithName Nothing


-- buildGraphWithName builds a node for the given expression with the name if
-- provided, otherwise a unique generated name, returning the name of the node
buildGraphWithName :: Maybe Name -> Expr -> State FunctionState Name
buildGraphWithName mn (Lit lit) = do
    name <- Maybe.maybe depName return mn
    -- create DExpr
    dexpr <- DLit <$> (case lit of
        LInt i  -> return $ DInt i
        LBool b -> return $ DBool b
        -- generate the atomic expressions in the list, or get the variables
        -- if they are non-atomic
        LList ls -> DList <$> mapM buildExpr ls)
    -- setup the node
    setupExpressionNode name dexpr
    return name
buildGraphWithName mn (Var var) = do
    name <- Maybe.maybe depName return mn
    -- just create a new node referring to an existing var
    -- assuming that it has already been created
    -- TODO: some way of determining whether this is actually someFunc/0
    setupExpressionNode name (DVar var)
    return name
buildGraphWithName mn e @ Op{} = do
    name <- Maybe.maybe depName return mn
    -- operations are atomic, and will be the combination of multiple atomic
    -- expressions or already-calculated Vars, so we can combine them
    dexpr <- buildOpExpr e
    setupExpressionNode name dexpr
    return name
buildGraphWithName mn e @ App{} = do
    name <- Maybe.maybe depName return mn
    -- collect the name of the function and its given arguments
    let (fName, args) = collectApp e
    -- build the nodes for the arguments
    dexpr <- DApp fName <$> mapM buildExpr args
    -- setup this node
    setupExpressionNode name dexpr
    return name
buildGraphWithName mn (Let defs e) = do
    -- build the nodes for the definitions
    mapM_ (\(Def defName defE) -> buildGraphWithName (Just defName) defE) defs
    -- then build node for the expression
    buildGraphWithName mn e
buildGraphWithName mn (If e1 e2 e3) = do
    name <- Maybe.maybe depName return mn
    -- setup the condition
    condDExpr <- buildExpr e1
    addDependencyNode name (Conditional condDExpr)
    setupDependencies name condDExpr
    let -- buildBranch creates the scope and expression for the given branch
        buildBranch e = do
            oldScope <- addScope >>= setScope
            buildGraph e >>= addScopeDependency
            rearrangeExternalDependencies 
            setScope oldScope
        -- rearrangeExternalDependencies handles situations where an expression
        -- within a certain scope depends on a let definition outside of this
        -- scope -- e.g. let ... in (if ...)
        -- this would create a dependency arc from outside the scope, to inside
        -- the scope, which does not work with the current code generation
        -- algorithm (TODO?)
        -- instead, we check if any nodes in the current scope and below have
        -- dependencies to some node outside of the scopes, and route that
        -- to link to the Condition node we are currently generating, ensuring
        -- that there are no inter-scope connections
        rearrangeExternalDependencies = do
            ((ns, ds), params, counter, scope) <- get
            internal <- internalNodes scope
            let externalDeps = Set.filter 
                    (\(p, c, _) -> Set.member c internal 
                        && Set.notMember p internal)
                    ds
                ds' = Set.difference ds externalDeps
            put ((ns, ds'), params, counter, scope)
            mapM_ (\(p, _, _) -> addDependency name p) (Set.toList externalDeps)
    -- setup branches
    thenScope <- buildBranch e2
    elseScope <- buildBranch e3
    -- add as special dependents of the Condition
    addConditionalDependency name thenScope elseScope
    return name


-- internalNodes gets the set of all nodes which are descendents of the
-- given node
internalNodes :: Name -> State FunctionState (Set Name)
internalNodes name = do
    ((_, ds), _, _, _) <- get
    let bfs :: Set Name -> [Name] -> Set Name
        bfs visited  [] 
            = visited
        bfs visited (n : ns)
            | Set.member n visited = bfs visited ns
            | otherwise            = bfs (Set.insert n visited) (ns ++ children)
            where children = Set.toList 
                    $ Set.map (\(_, c, _) -> c) 
                    $ Set.filter (\(p, _, _) -> p == n) ds
    return $ bfs Set.empty [name]
