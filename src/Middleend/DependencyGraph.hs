module Middleend.DependencyGraph where

import Hashkell.Syntax

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
    | DAtomApp Name [DExpr]
    | DHighApp Name DExpr [DExpr]
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
type MergeAtomic = Bool

data BuildingState = BuildingState 
    { dependencyGraph :: DependencyGraph
    , params          :: Params
    , counter         :: Counter
    , currentScope    :: CurrentScope
    , mergeAtomic     ::  MergeAtomic
    , trivialityTable  :: Map Name Bool
    }

-- createDependencyGraph builds the dependency graph of a function with the
-- given params and definition
createDependencyGraph :: MergeAtomic -> Map Name Bool -> [Name] -> Expr -> DependencyGraph
createDependencyGraph ma tt params defn 
    = dependencyGraph finalState
    where
        initState = BuildingState
            { dependencyGraph = (Map.fromList [("_", Scope)], Set.empty)
            , params = params
            , counter = 0
            , currentScope = "_"
            , mergeAtomic = ma
            , trivialityTable = tt
            } 
        finalState = execState (buildGraph defn) initState

-- setScope sets the current scope to the given scope, and returns the previous
-- scope in the state.
setScope :: Name -> State BuildingState Name
setScope s = do
    bs <- get
    put (bs { currentScope = s })
    return (currentScope bs)


-- hasParent checks if the node with the given name has any parent nodes.
hasParent :: Name -> State BuildingState Bool
hasParent name = do
    bs <- get
    let (_, ds) = dependencyGraph bs
        scope   = currentScope bs
        dependencies = Set.filter (\(_, c, t) -> c == name && t == Dep) ds
    scopedPs <- filterM (\(p, _, _) -> p `isDescendentOf` scope) 
        (Set.toList dependencies)
    return (not (null scopedPs))


-- addArc inserts a dependency arc into the state
addArc :: Dependency -> State BuildingState ()
addArc d = do
    bs <- get
    let (ns, ds) = dependencyGraph bs
    put (bs { dependencyGraph = (ns, Set.insert d ds) })


-- isDescendentOf checks if a is a descendent of b by recursively checking
-- a's parents for b
isDescendentOf :: Name -> Name -> State BuildingState Bool
a `isDescendentOf` b = do
    bs <- get
    let (_, ds) = dependencyGraph bs
        pes = Set.filter (\(_, c, t) -> c == a && t == Dep) ds
        ps  = Set.toList $ Set.map (\(p, _, _) -> p) pes
    parentsAreDescendent <- or <$> mapM (`isDescendentOf` b) ps
    -- b being an element of ps is handled in the next call
    return (a == b || parentsAreDescendent)

-- removeSimilarDependencies removes any dependency arcs which will be covered
-- transitively if we were to add the given arc
-- does not remove that arc (if it exists), but will not be a problem since
-- the dependencies are stored in a Set
removeSimilarDependencies :: Name -> Name -> State BuildingState ()
removeSimilarDependencies child parent = do
    bs <- get
    let (ns, ds) = dependencyGraph bs
        similars = Set.toList $ Set.filter 
            (\(p, c, _) -> c == child && p /= parent)
            ds
    similars' <- Set.fromList 
        <$> filterM (\(p, _, _) -> parent `isDescendentOf` p) similars
    put ( bs { dependencyGraph = (ns, Set.difference ds similars') })


-- addDependency adds an arc from parent to child into the state
addDependency :: Name -> Name -> State BuildingState ()
addDependency child parent = do
    bs <- get
    let p = if parent `elem` params bs then currentScope bs else parent
    -- check if we already have the dependency, or it exists transitively
    depAlreadyExists <- child `isDescendentOf` p
    unless depAlreadyExists $ do
        -- if it does not exist transitively, add the arc then remove any
        -- dependencies which are now redundandant through transitivity
        addArc (p, child, Dep)
        removeSimilarDependencies child p


-- addScopeDependency explicitly adds an arc from the current scope to the 
-- child, if the child is not already a descendent of the scope
addScopeDependency :: Name -> State BuildingState ()
addScopeDependency child = do
    scope <- currentScope <$> get
    alreadyDescendent <- child `isDescendentOf` scope
    unless alreadyDescendent $ addArc (scope, child, Dep)


-- addConditionalDependency sets up the dependency arcs for a conditional node,
-- given its 'then' and 'else' scope nodes.
addConditionalDependency :: Name -> Name -> Name -> State BuildingState ()
addConditionalDependency c t e = do
    addArc (c, e, DepElse)
    addArc (c, t, DepThen)


-- addDependencyNode inserts a node with the given name into the state.
addDependencyNode :: Name -> DNode -> State BuildingState ()
addDependencyNode name node = do
    bs <- get
    let (ns, ds) = dependencyGraph bs
    put ( bs { dependencyGraph = (Map.insert name node ns, ds) })


-- addScope inserts a scope node into the state using the current counter,
-- increments the counter, then returns the generated name.
addScope :: State BuildingState Name
addScope = do
    bs <- get
    let c = counter bs
        name = "_" ++ show c
    put ( bs { counter = c + 1 })
    addDependencyNode name Scope
    return name


-- depName returns a new generated identifier name using the current counter
-- value, then increments it for next use
depName :: State BuildingState Name
depName = do
    bs <- get
    let c = counter bs
        name = "_x" ++ show c
    put ( bs { counter = c + 1 })
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
dependenciesFromDExpr (DAtomApp _ es)
    = concatMap dependenciesFromDExpr es
dependenciesFromDExpr (DHighApp _ e es)
    = dependenciesFromDExpr e ++ concatMap dependenciesFromDExpr es

-- setupDependencies adds all the dependencies for a node with the given name
-- and DExpr
setupDependencies :: Name -> DExpr -> State BuildingState ()
setupDependencies name dexpr = do
    let parents = dependenciesFromDExpr dexpr
    if null parents
        -- add a scope dependency to ensure that they stay within their scope
        then addScopeDependency name
        else mapM_ (addDependency name) parents >> addScopeDependency name

-- setupExpressionNode setups up the creation of a node and its dependencies
setupExpressionNode :: Name -> DExpr -> State BuildingState ()
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
buildOpExpr :: Expr -> State BuildingState DExpr
buildOpExpr (Op op e1 e2)
    = case (atomicToDExpr e1, atomicToDExpr e2) of
        (Nothing, Nothing)   -> DOp op <$> buildOpExpr e1 <*> buildOpExpr e2
        (Just de1, Nothing)  -> DOp op de1 <$> buildOpExpr e2
        (Nothing, Just de2)  -> DOp op <$> buildOpExpr e1 <*> return de2
        (Just de1, Just de2) -> return $ DOp op de1 de2
buildOpExpr e
    = buildExpr e

-- isTrivialFunction returns True if the given name is the name of an atomic
-- function, i.e. a O(1) in-built function, or of a function which the user
-- assigned low complexity
isTrivialFunction :: Name -> State BuildingState Bool
isTrivialFunction n
    = (||) isAtomic <$> isTrivial
    where
        -- NOTE: incomplete list of functions
        isAtomic  = n `elem` ["null", "head", "tail"]
        isTrivial = Maybe.fromMaybe False 
            <$> (Map.lookup n . trivialityTable <$> get)

-- buildExpr converts atomics to expressions, builds nodes for non-atomics
buildExpr :: Expr -> State BuildingState DExpr
buildExpr e = do
    ma <- mergeAtomic <$> get
    if not ma
        then DVar <$> buildGraph e
        else 
            case (atomicToDExpr e, e) of
                -- if atomic, then can just return dexpr for embedding
                (Just de, _)    -> return de
                (_, e' @ App{}) -> do
                    let (fName, args) = collectApp e
                    isTrivial <- isTrivialFunction fName
                    if isTrivial
                        -- if the function is atomic or trivial to compute,
                        -- then can just return dexpr for embedding
                        then DAtomApp fName <$> mapM buildExpr args
                        -- otherwise, build the node as usual
                        else buildNode
                -- if not atomic, build the node
                _               -> buildNode
            where buildNode = DVar <$> buildGraph e


-- buildGraph builds a node for the given expression with a unique
-- generated name
buildGraph :: Expr -> State BuildingState Name
buildGraph = buildGraphWithName Nothing


-- buildGraphWithName builds a node for the given expression with the name if
-- provided, otherwise a unique generated name, returning the name of the node
buildGraphWithName :: Maybe Name -> Expr -> State BuildingState Name
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
    let (fName, args @ (a : as)) = collectApp e
    -- build the nodes for the arguments
    isTrivial <- isTrivialFunction fName
    dexpr <- if isTrivial
        then DAtomApp fName <$> mapM buildExpr args
        else if fName `elem` ["map", "zipWith"]
            then do
                let (fName', args') = case a of 
                        App{} -> collectApp a 
                        Var n ->  (n, [])
                isTrivial' <- isTrivialFunction fName'
                let dfunc = if isTrivial' then DAtomApp else DApp
                DHighApp fName <$> (dfunc fName' <$> mapM buildExpr args') <*> mapM buildExpr as
            else DApp fName <$> mapM buildExpr args
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
            bs <- get
            let (ns, ds) = dependencyGraph bs
                scope    = currentScope bs
            internal <- internalNodes scope
            let externalDeps = Set.filter 
                    (\(p, c, _) -> Set.member c internal 
                        && Set.notMember p internal)
                    ds
                ds' = Set.difference ds externalDeps
            put ( bs { dependencyGraph = (ns, ds') })
            mapM_ (\(p, _, _) -> addDependency name p) (Set.toList externalDeps)
    -- setup branches
    thenScope <- buildBranch e2
    elseScope <- buildBranch e3
    -- add as special dependents of the Condition
    addConditionalDependency name thenScope elseScope
    return name


-- internalNodes gets the set of all nodes which are descendents of the
-- given node
internalNodes :: Name -> State BuildingState (Set Name)
internalNodes name = do
    ds <- snd . dependencyGraph <$> get
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
