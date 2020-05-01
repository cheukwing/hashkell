{-# LANGUAGE TupleSections #-}

module Middleend.DependencyGraph where

import Hashkell.Syntax
import Context

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


data BuildingState = BuildingState 
    { dependencyGraph :: DependencyGraph
    , params          :: [Name]
    , counter         :: Int
    , currentScope    :: Name
    , mergeAtomic     :: Bool
    , trivialityTable :: Map Name Bool
    }

-- createDependencyGraph builds the dependency graph of a function with the
-- given params and definition
createDependencyGraph :: Context -> Map Name Bool -> [Name] -> Expr -> DependencyGraph
createDependencyGraph ctx tt params defn 
    = if noRedundantArcs ctx then removeRedundantArcs dg else dg
    where
        dg = dependencyGraph finalState
        finalState = execState (buildGraph defn) initState
        initState = BuildingState
            { dependencyGraph = (Map.fromList [("_", Scope)], Set.empty)
            , params = params
            , counter = 0
            , currentScope = "_"
            , mergeAtomic = fewerAtomicNodes ctx
            , trivialityTable = tt
            } 


-- removeRedundantArcs removes the redundant arcs for each node, i.e. arcs
-- which describe transitive relations, by finding which of each nodes children
-- can be reached from its other children
removeRedundantArcs :: DependencyGraph -> DependencyGraph
removeRedundantArcs (ns, ds)
    = (ns, Set.difference ds arcsToRemove)
    where
        -- accumulatively build up a set of all the arcs that are redundant
        arcsToRemove = foldl findArcsToRemove Set.empty (Map.keys ns)
        -- get all the descendents of a node
        descendents = Map.mapWithKey (\n _ -> strictDescendentsOf n) ns
        strictDescendentsOf n = Set.delete n (internalNodes ds n)
        -- get all the direct children of a node
        childrens   = Map.mapWithKey (\n _ -> childrenOf n) ns
        childrenOf n = Set.map (\(_, c, _) -> c) 
            $ Set.filter (\(p, _, t) -> p == n && t == Dep) ds
        -- for each node, add all of its redundant arcs to the accumulative set
        findArcsToRemove :: Set Dependency -> Name -> Set Dependency
        findArcsToRemove deps n
            = Set.unions (deps : Set.toList (Set.map findArcsToRemove children))
            where
                children = (Map.!) childrens n
                -- if a child node can be reached by another child, then the
                -- arc to the former is redundant
                findArcsToRemove :: Name -> Set Dependency
                findArcsToRemove child
                    = Set.map (n, , Dep) $ Set.filter (`Set.member` descendent) children
                    where descendent = (Map.!) descendents child


-- setScope sets the current scope to the given scope, and returns the previous
-- scope in the state.
setScope :: Name -> State BuildingState Name
setScope s = do
    bs <- get
    put (bs { currentScope = s })
    return (currentScope bs)


-- addArc inserts a dependency arc into the state
addArc :: Dependency -> State BuildingState ()
addArc d = do
    bs <- get
    let (ns, ds) = dependencyGraph bs
    put (bs { dependencyGraph = (ns, Set.insert d ds) })


-- addDependency adds an arc from parent to child into the state
addDependency :: Name -> Name -> State BuildingState ()
addDependency child parent = do
    bs <- get
    let p = if parent `elem` params bs then currentScope bs else parent
    addArc (p, child, Dep)


-- addScopeDependency adds an arc from the current scope to the child
addScopeDependency :: Name -> State BuildingState ()
addScopeDependency child =
    currentScope <$> get >>= addDependency child


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
{-
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
-}
    
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


-- buildExpr combines several operations expressions so that they will
-- appear as a single node, avoiding nodes for each bracketing
-- as in e.g. ((x + y) + z) + w causing 3 nodes
{-
buildExpr :: Expr -> State BuildingState DExpr
buildExpr (Op op e1 e2) = do
    ma <- mergeAtomic <$> get
    let build = if ma then buildExpr else buildExpr
    DOp op <$> build e1 <*> build e2
buildExpr e
    = buildExpr e
-}

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
        else case e of
            Lit (LInt i) -> 
                return $ DLit (DInt i)
            Lit (LBool b) ->
                return $ DLit (DBool b)
            Lit (LList ls) ->
                DLit <$> (DList <$> mapM buildExpr ls)
            Var n ->
                return $ DVar n
            Op op e1 e2 ->
                -- handles embedding atomics interleaved between non-atomics
                DOp op <$> buildExpr e1 <*> buildExpr e2
            App _ _ -> do
                let (fName, args) = collectApp e
                isTrivial <- isTrivialFunction fName
                if isTrivial
                    then DAtomApp fName <$> mapM buildExpr args
                    else DVar <$> buildGraph e
            _ ->
                DVar <$> buildGraph e


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
buildGraphWithName mn (Op op e1 e2) = do
    name <- Maybe.maybe depName return mn
    dexpr <- DOp op <$> buildExpr e1 <*> buildExpr e2
    setupExpressionNode name dexpr
    return name
buildGraphWithName mn e @ App{} = do
    name <- Maybe.maybe depName return mn
    -- collect the name of the function and its given arguments
    let (fName, args @ (a : as)) = collectApp e
    isTrivial <- isTrivialFunction fName
    dexpr <- if isTrivial
        -- if trivial -> DAtomApp
        then DAtomApp fName <$> mapM buildExpr args
        else if fName `elem` ["map", "zipWith"]
            then do
                let (fName', args') = case a of 
                        App{} -> collectApp a 
                        Var n ->  (n, [])
                isTrivial' <- isTrivialFunction fName'
                let dfunc = if isTrivial' then DAtomApp else DApp
                -- if higher order function -> DHighApp
                DHighApp fName <$> (dfunc fName' <$> mapM buildExpr args') <*> mapM buildExpr as
            -- anything else -> DApp
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
        -- we check if any nodes in the current scope and below have
        -- dependencies to some node outside of the scopes, and route that
        -- to link to the Condition node we are currently generating, ensuring
        -- that there are no inter-scope connections
        rearrangeExternalDependencies = do
            bs <- get
            let (ns, ds) = dependencyGraph bs
                scope    = currentScope bs
                internal = internalNodes ds scope
                externalDeps = Set.filter 
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
internalNodes :: Dependencies -> Name -> Set Name
internalNodes ds name
    = bfs Set.empty [name]
    where 
        bfs :: Set Name -> [Name] -> Set Name
        bfs visited  [] 
            = visited
        bfs visited (n : ns)
            | Set.member n visited = bfs visited ns
            | otherwise            = bfs (Set.insert n visited) (ns ++ children)
            where children = Set.toList 
                    $ Set.map (\(_, c, _) -> c) 
                    $ Set.filter (\(p, _, _) -> p == n) ds
