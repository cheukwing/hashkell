module DependencyGraph.Internal where 

import Simple.Syntax

import Control.Monad.State.Strict
import Data.Either (Either(..), either)
import qualified Data.Either as Either
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (Maybe)
import qualified Data.Maybe as Maybe
import Data.List (intercalate)

-- For drawing graph
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.GraphViz as G
import qualified Data.GraphViz.Attributes.Complete as G
import qualified Data.GraphViz.Types as G


type DependencyGraph = (NodeTable, Dependencies)
type NodeTable = Map DName DNode
type Dependencies = Set Dependency
type Dependency = (DName, DName, DType)

type DName = String

data DType
    = DepD
    | DepThen
    | DepElse
    | DepArg
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
    | DLit Lit
    deriving Eq

instance Show DExpr where
    show (DLit lit) = show lit
    show (DVar name) = name
    show (DOp op e1 e2) = "(" ++ unwords [show e1, show op, show e2] ++ ")"
    show (DApp n es) = "(" ++ unwords (n : map show es) ++ ")"



--- GRAPH BUILDING ---

type Args = [Name]
type Counter = Int
type CurrentScope = DName
type FunctionState = (DependencyGraph, Args, Counter, CurrentScope)

-- depName returns a unique identifier based on the current counter.
depName :: State FunctionState DName
depName = do
    (_, _, counter, _) <- get
    return ("_x" ++ show counter)


-- scopeName returns an unique name for the current scope based on the current
-- counter. 
scopeName :: State FunctionState DName
scopeName = do
    (_, _, counter, _) <- get
    return ("_" ++ show counter)


-- setScope sets the current scope to the given scope, and returns the previous
-- scope in the state.
setScope :: DName -> State FunctionState DName
setScope s = do
    (graph, args, counter, scope) <- get
    put (graph, args, counter, s)
    return scope


-- hasParent checks if the node with the given name has any parent nodes.
hasParent :: DependencyGraph -> DName -> Bool
hasParent (_, ds) name =
    not $ null $ Set.filter (\(_, c, t) -> c == name && t == DepD) ds


-- addArc inserts a dependency arc into the state
addArc :: Dependency -> State FunctionState ()
addArc d = do
    ((ns, ds), args, counter, scope) <- get
    put ((ns, Set.insert d ds), args, counter, scope)


-- isDescendentOf checks if a is a descendent of b by recursively checking
-- a's parents for b
isDescendentOf :: DName -> DName -> State FunctionState Bool
a `isDescendentOf` b = do
    ((_, ds), _, _, _) <- get
    let 
        pes = Set.filter (\(_, c, t) -> c == a && t == DepD) ds
        ps  = Set.toList $ Set.map (\(p, _, _) -> p) pes
    parentsAreDescendent <- or <$> mapM (`isDescendentOf` b) ps
    -- b being an element of ps is handled in the next call
    return (a == b || parentsAreDescendent)


-- addDependency adds an arc from parent to child into the state
-- TODO: If a->b and c = a + b, then we should only have one dependency
-- from b->c, instead of a->c and b->c
addDependency :: DName -> DName -> State FunctionState ()
addDependency parent child = do
    (graph, args, counter, scope) <- get
    if parent `elem` args then do
        -- setup an arc to signify a use of an argument in this node
        addArc ("_", child, DepArg)
        depAlreadyExists <- child `isDescendentOf` scope
        if depAlreadyExists
            then return ()
            else addArc (scope, child, DepD)
    else do
        depAlreadyExists <- child `isDescendentOf` parent
        if depAlreadyExists
            then return ()
            else addArc (parent, child, DepD)


-- addScopeDependency explicitly adds an arc from the current scope to the 
-- child, if such an arc does not already exist
addScopeDependency :: DName -> State FunctionState ()
addScopeDependency child = do
    (graph, args, counter, scope) <- get
    if hasParent graph child 
        then return ()
        else addArc (scope, child, DepD)


-- addConditionalDependency sets up the dependency arcs for a conditional node,
-- given its 'then' and 'else' scope nodes.
addConditionalDependency :: DName -> DName -> DName -> State FunctionState ()
addConditionalDependency c t e = do
    addArc (c, e, DepElse)
    addArc (c, t, DepThen)


-- addDependencyNode inserts a node with the given name into the state.
addDependencyNode :: DName -> DNode -> State FunctionState ()
addDependencyNode name node = do
    ((ns, ds), args, counter, scope) <- get
    put ((Map.insert name node ns, ds), args, counter, scope)


-- addScope inserts a scope node into the state using the current counter,
-- then returns the generated name.
addScope :: State FunctionState DName
addScope = do
    name <- scopeName
    addDependencyNode name Scope
    return name


-- incrementCounter increments the current counter, while also returning the
-- depName generated from the previous counter for reference.
incrementCounter :: State FunctionState DName
incrementCounter = do
    (graph, args, counter, scope) <- get
    name <- depName
    put (graph, args, counter + 1, scope)
    return name


-- createDependencyGraph creates the dependency graph with the given argument
-- names and expression.
createDependencyGraph :: [String] -> Expr -> DependencyGraph
createDependencyGraph args defn
    = case xn of
        --Left x -> (Map.fromList [("_", Expression x)], [])
        Left x -> ( Map.fromList [("_", Scope), ("_x0", Expression x)]
                  , Set.fromList [("_", "_x0", DepD)] )
        _      -> graph
    where
        (xn, (graph, _, _, _))  = runState (buildGraph defn) initState
        initState = ((Map.fromList [("_", Scope)], Set.empty), args, 0, "_")


-- buildGraph recursively builds a graph from the given expression,
-- each call returns either the dependency expression generated if it is a
-- atomic expression (trivial to compute), or the name of the node in which
-- the value is stored.
buildGraph :: Expr -> State FunctionState (Either DExpr DName)
buildGraph (Lit lit) =
    -- literals are atomic
    return (Left (DLit lit))
buildGraph (Var var) =
    -- assume: var is already defined
    return (Right var)
buildGraph (Op op e1 e2) = do
    xn1 <- buildGraph e1
    xn2 <- buildGraph e2
    case (xn1, xn2) of
        -- if both arguments atomic, return as atomic
        (Left x1, Left x2) ->
            return $ Left (DOp op x1 x2)
        -- otherwise, setup dependencies for expression
        _                  -> do
            -- one of the arguments may be atomic, if so,
            -- do not bother adding as dependent
            name <- depName
            let addDependencyIfName = either return
                        (\n -> addDependency n name >> return (DVar n))
            v1 <- addDependencyIfName xn1
            v2 <- addDependencyIfName xn2
            addDependencyNode name (Expression (DOp op v1 v2))
            Right <$> incrementCounter
buildGraph e @ App{} = do
    let
        (appName, args) = appArgs e
        -- assume: all function applications are full
        -- extract the function name and the arguments
        appArgs :: Expr -> (Name, [Expr])  
        appArgs (App (Var name) e)
          = (name, [e])
        appArgs (App e1 e2)
          = (name, e2 : es)
          where (name, es) = appArgs e1
    ds <- mapM buildGraph args
    name <- depName
    if all Either.isLeft ds then do
        -- if all arguments are atomic, then only need to add as dependent 
        -- to the scope, so the node has a parent
        addScopeDependency name
        addDependencyNode name (Expression (DApp appName (Either.lefts ds)))
        Right <$> incrementCounter
    else do
        -- otherwise, will need to set self as dependent to non-atomics
        mapM_ (`addDependency` name) (Either.rights ds)
        addDependencyNode name (Expression (DApp appName (map (either id DVar) ds)))
        Right <$> incrementCounter
buildGraph (Let defs e) = do
    let
        -- assume: definition names are globally unique throughout all paths
        defToGraph :: Def -> State FunctionState ()
        defToGraph (Def defName exp) = do
            xn <- buildGraph exp
            -- if definition is atomic, then set as dependent to scope
            -- otherwise set dependencies as normal
            let addDependencyIfName = either 
                    (\x -> addScopeDependency defName >> return x)
                    (\n -> addDependency n defName >> return (DVar n))
            v <- addDependencyIfName xn
            addDependencyNode defName (Expression v)
    mapM_ defToGraph defs
    xn <- buildGraph e
    -- if expression is atomic, return as atomic
    -- otherwise, add setup dependencies as normal
    name <- depName
    either
        (return . Left)
        (\n -> do
            addDependency n name
            addDependencyNode name (Expression (DVar n))
            Right <$> incrementCounter)
        xn
buildGraph (If e1 e2 e3) = do
    let 
        handleBranch e = do
            scope <- addScope
            incrementCounter
            oldScope <- setScope scope
            xn <- buildGraph e
            -- if the expression is atomic, then create a new node and set its
            -- parent to the scope
            -- otherwise can just set its parent to the scope
            either handleExpr addScopeDependency xn
            setScope oldScope

        handleExpr e = do
            name <- depName
            addScopeDependency name
            addDependencyNode name (Expression e)
            incrementCounter
            return ()

    cxn <- buildGraph e1
    thenScope <- handleBranch e2
    elseScope <- handleBranch e3
    -- restore scope, setup condition dependencies
    name <- depName
    addConditionalDependency name thenScope elseScope
    either
        (\x -> do 
            addDependencyNode name (Conditional x)
            Right <$> incrementCounter)
        (\n -> do 
            addDependency n name 
            addDependencyNode name (Conditional (DVar n))
            Right <$> incrementCounter)
        cxn

--- CODE GENERATION ---

type GeneratedNames = Set DName
type ParallelisedScope = Bool
type GenerationState = (DependencyGraph, GeneratedNames, ParallelisedScope)


-- getChildren returns a set of all children of the given node, except those
-- with conditional dependencies.
getChildren :: DName -> State GenerationState (Set DName)
getChildren n = do
    ((_, ds), _, _) <- get
    return $ 
        Set.map (\(_, c, _) -> c) $
        Set.filter (\(p, _, t) -> p == n && t == DepD) ds


-- getParents returns a set of all parents of the given node, except those
-- with conditional dependencies.
getParents :: DName -> State GenerationState (Set DName)
getParents n = do
    ((_, ds), _, _) <- get
    return $ 
        Set.map (\(p, _, _) -> p) $
        Set.filter (\(_, c, t) -> c == n && t == DepD) ds


-- getBranch finds the name of the node which is a branch of the given node
-- and has the given dependency type. Used for finding the conditional
-- dependencies, and not for regular dependencies.
getBranch :: DName -> DType -> State GenerationState DName
getBranch p t = do
    ((_, ds), _, _) <- get
    let bs = Set.filter (\(p', _, t') -> p == p' && t == t') ds
    if Set.size bs == 1
        then return $ ((\(_, c, _) -> c) . Set.elemAt 0) bs
        else error "Multiple branches with the same type or branch does not exist"


-- satisifiedChildren returns the children of the given node whose 
-- dependencies are met, i.e. their code has been generated.
satisfiedChildren :: DName -> State GenerationState [DName]
satisfiedChildren name = do
    (_, gns, _) <- get
    children <- Set.toList <$> getChildren name
    cps <- zip children <$> mapM getParents children
    return [c | (c, ps) <- cps, Set.isSubsetOf ps gns]


-- getNode retrieves the node with the given name.
getNode :: DName -> State GenerationState DNode
getNode name = do
    ((ns, _), _, _) <- get
    return (ns Map.! name)


-- updateGeneratedNames records that a node with the given name has had its
-- code generated.
updateGeneratedNames :: DName -> State GenerationState ()
updateGeneratedNames gn = do
    (g, gns, par) <- get
    put (g, Set.insert gn gns, par)


-- encodeDependencyGraph generates code from the given dependency graph,
-- either encoding it sequentially if False, else parallelised.
encodeDependencyGraph :: DependencyGraph -> Bool -> String
encodeDependencyGraph g False
    = snd $ evalState (generateSequentialCode "_") (g, Set.empty, False)
encodeDependencyGraph g True
    = snd $ evalState (generateParallelCode "_") (g, Set.empty, False)


generateSequentialCode :: DName -> State GenerationState (DName, String)
generateSequentialCode name = do
    updateGeneratedNames name
    node <- getNode name
    -- generate the code for the children of the current node, if all other
    -- dependencies for that child have already been generated
    let generateChildren = do
            (lns, cs) <- unzip <$> (satisfiedChildren name >>= mapM generateSequentialCode)
            let mLastName = if null lns then Nothing else Just (last lns)
            return (mLastName, intercalate "; " cs)
    case node of
        Scope -> do
            -- Scope is encoded as `let {children} in {final descendent}`
            (mLastName, code) <- generateChildren
            let lastName = Maybe.fromJust mLastName
            return (lastName, "let " ++ code ++ " in " ++  lastName)
        Expression e -> do
            -- Expression is encoded as `{name} = {e} {; children}`
            (mLastName, code) <- generateChildren
            let expCode = name ++ " = " ++ show e
            case mLastName of
                Just lastName -> return (lastName, expCode ++ "; " ++ code)
                Nothing       -> return (name, expCode)
        Conditional e -> do
            -- Conditional is encoded as 
            -- `{name} = if {e} then {then code} else {else code} ; {children}`
            (_, thenCode) <- getBranch name DepThen >>= generateSequentialCode
            (_, elseCode) <- getBranch name DepElse >>= generateSequentialCode
            (mLastName, code) <- generateChildren
            let endCode = name ++ " = if " ++ show e 
                            ++ " then " ++ thenCode 
                            ++ " else " ++ elseCode 
                            ++ "; " ++ code
            return (Maybe.fromMaybe name mLastName, endCode)


-- setParallelisedScope sets whether the current code in the current scope
-- in the graph should generate parallel code.
setParallelisedScope :: Bool -> State GenerationState Bool
setParallelisedScope par = do
    (g, gns, par') <- get
    put (g, gns, par)
    return par'


-- getParallelisedScope retrieves the bool determining whether the current code
-- in the current scope in the graph should be parallelised.
getParallelisedScope :: State GenerationState Bool
getParallelisedScope = do
    (_, _, par) <- get
    return par


-- scopeContainsParallelism returns whether the current scope in the graph
-- contains any nodes which should be generated in parallel, i.e. if it
-- contains any function calls.
-- TODO: do not parallelise if only one path
scopeContainsParallelism :: DName -> State GenerationState Bool
scopeContainsParallelism start =
    or <$> (Set.toList <$> getChildren start >>= mapM scopeContainsParallelism')
    where 
        scopeContainsParallelism' :: DName -> State GenerationState Bool
        scopeContainsParallelism' name = do
            node <- getNode name
            case node of
                Scope              -> return False
                Expression DApp{}  -> return True
                Conditional DApp{} -> return True
                _                  ->
                    or <$> (Set.toList <$> getChildren name >>= mapM scopeContainsParallelism')

    
generateParallelCode :: DName -> State GenerationState (DName, String)
generateParallelCode name = do
    updateGeneratedNames name
    node <- getNode name
    let generateChildren = do
            (lns, cs) <- unzip <$> (satisfiedChildren name >>= mapM generateParallelCode)
            let mLastName = if null lns then Nothing else Just (last lns)
            return (mLastName, intercalate "; " cs)
    case node of
        Scope -> do
            par <- scopeContainsParallelism name
            oldPar <- setParallelisedScope par
            (mLastName, code) <- generateChildren
            let lastName = Maybe.fromJust mLastName
            setParallelisedScope oldPar
            let finalCode = if par
                then "runEval $ do { " ++ code ++ "; return " ++  lastName ++ "}"
                else "let " ++ code ++ " in " ++ lastName
            return (lastName, finalCode)
        Expression e -> do
            (mLastName, code) <- generateChildren
            par <- getParallelisedScope
            let expCode = case (par, e) of
                    (_, DApp f args) -> 
                        name ++ " <- rpar (" ++ unwords (f : map show args) ++ ")"
                    (True, _)        ->
                        "let { " ++ name ++ " = " ++ show e ++ " }"
                    _                ->
                        name ++ " = " ++ show e
            case mLastName of
                Just lastName -> return (lastName, expCode ++ "; " ++ code)
                Nothing       -> return (name, expCode)
        Conditional e -> do
            (_, thenCode) <- getBranch name DepThen >>= generateParallelCode
            (_, elseCode) <- getBranch name DepElse >>= generateParallelCode
            (mLastName, code) <- generateChildren
            par <- getParallelisedScope
            let condCode = name ++ " = if " ++ show e 
                            ++ " then " ++ thenCode 
                            ++ " else " ++ elseCode 
            let endCode = if par then "let { " ++ condCode ++ " }" else condCode
            return (Maybe.fromMaybe name mLastName, endCode ++ "; " ++ code)


--- GRAPH DRAWING ---

drawDependencyGraph :: String -> DependencyGraph -> IO ()
drawDependencyGraph fileName (ns, ds) = do
    let 
        dotGraph = G.graphElemsToDot depGraphParams (Map.toList ns) (Set.toList ds)
        dotText = G.printDotGraph dotGraph
    TL.writeFile fileName dotText


-- Parameters for GraphViz
depGraphParams :: G.GraphvizParams DName DNode DType () DNode
depGraphParams = G.defaultParams {
    G.fmtNode = \(name, node) -> case node of
        Scope         -> [G.toLabel name]
        Expression e  -> [G.toLabel $ name ++ " = " ++ show e]
        Conditional e -> [G.toLabel $ name ++ " = If " ++ show e]
    , G.fmtEdge = \(_, _, t) -> case t of
        DepD -> []
        DepThen -> [G.toLabel "then"]
        DepElse -> [G.toLabel "else"]
        DepArg  -> [G.style G.dotted]
}