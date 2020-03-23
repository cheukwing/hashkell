module DependencyGraph (
    DependencyGraph,
    toDependencyGraph,
    drawDependencyGraph,
) where

import Simple.Syntax

import Control.Monad.State.Strict
import Data.Either (Either(..), either)
import qualified Data.Either as Either
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- For drawing graph
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.GraphViz as G
import qualified Data.GraphViz.Attributes.Complete as G
import qualified Data.GraphViz.Types as G



type DependencyGraph = (NodeTable, [Dependency])
type NodeTable = Map DName DNode
type Dependency = (DName, DName, DType)

type DName = String

data DType
    = DepD
    | DepThen
    | DepElse
    deriving (Eq, Show)

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

type FunctionState = (DependencyGraph, [Name], Int, DName)


--- GRAPH BUILDING ---

depName :: State FunctionState DName
depName = do
    (_, _, counter, _) <- get
    return ("_x" ++ show counter)


scopeName :: State FunctionState DName
scopeName = do
    (_, _, counter, _) <- get
    return ("_" ++ show counter)


setScope :: DName -> State FunctionState DName
setScope s = do
    (graph, args, counter, scope) <- get
    put (graph, args, counter, s)
    return scope


existsDependency :: DependencyGraph -> DName -> Bool
existsDependency (_, ds) name =
    any (\(_, child, _) -> child == name) ds


addArc :: DependencyGraph -> DName -> DName -> DependencyGraph
addArc (ns, ds) parent child =
    (ns, (parent, child, DepD) : ds)


addDependency :: DName -> DName -> State FunctionState ()
addDependency parent child = do
    (graph, args, counter, scope) <- get
    if parent `elem` args then
        put (addArc graph scope child, args, counter, scope)
    else
        put (addArc graph parent child, args, counter, scope)


addScopeDependency :: DName -> State FunctionState ()
addScopeDependency child = do
    (graph, args, counter, scope) <- get
    -- TODO: check if equivalent to old method
    if existsDependency graph child then
        return ()
    else
        put (addArc graph scope child, args, counter, scope)


addConditionalDependency :: DName -> DName -> DName -> State FunctionState ()
addConditionalDependency c t e = do
    ((ns, ds), args, counter, scope) <- get
    put ((ns, (c, t, DepThen) : (c, e, DepElse) : ds)
        , args, counter, scope)


addNode :: DependencyGraph -> DName -> DNode -> DependencyGraph
addNode (ns, ds) name node =
    (Map.insert name node ns, ds)


addDependencyNode :: DName -> DNode -> State FunctionState ()
addDependencyNode name node = do
    (graph, args, counter, scope) <- get
    put (addNode graph name node, args, counter, scope)


addScope :: State FunctionState DName
addScope = do
    (graph, args, counter, scope) <- get
    name <- scopeName
    put (addNode graph name Scope, args, counter + 1, scope)
    return name


incrementCounter :: State FunctionState DName
incrementCounter = do
    (graph, args, counter, scope) <- get
    name <- depName
    put (graph, args, counter + 1, scope)
    return name


toDependencyGraph :: [String] -> Expr -> DependencyGraph
toDependencyGraph args defn
    = case xn of
        Left x -> (Map.fromList [("_", Expression x)], [])
        _      -> graph
    where
        (xn, (graph, _, _, _))  = runState (buildGraph defn) initState
        initState = ((Map.fromList [("_", Scope)], []), args, 0, "_")


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
        -- to the scope, so we are not lost
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
            xn <- buildGraph  exp
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
        -- setup dependent of branches to the scope, to prevent unnecessary execution
        --setScopeAsParent = either handleExpr addScopeDependency
        handleExpr e = do
            name <- depName
            addScopeDependency name
            addDependencyNode name (Expression e)
            incrementCounter
            return ()

        handleBranch e = do
            scope <- addScope
            oldScope <- setScope scope
            xn <- buildGraph e
            either handleExpr addScopeDependency xn
            setScope oldScope

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

getChildren :: DependencyGraph -> DName -> [DName]
getChildren (_, ds) n
    = map (\(_, c, _) -> c) $ filter (\(n', _, _) -> n' == n) ds

getParents :: DependencyGraph -> DName -> [DName]
getParents (_, ds) n
    = map (\(p, _, _) -> p) $ filter (\(_, n', _) -> n' == n) ds


--- GRAPH DRAWING ---

drawDependencyGraph :: DependencyGraph -> IO ()
drawDependencyGraph (ns, ds) = do
    let 
        dotGraph = G.graphElemsToDot depGraphParams (Map.toList ns) ds
        dotText = G.printDotGraph dotGraph
    TL.writeFile "files.dot" dotText


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
}