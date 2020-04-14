module Backend.CodeGenerator where

import Simple.Syntax
import Middleend 
    ( DependencyGraph
    , EncodingInstructionTable
    , EncodingInstruction(..)
    , DNode(..)
    , DType(..)
    , DExpr(..)
    , DLit(..)
    )

import Prelude hiding (EQ, LT, GT)
import Data.List (intercalate)
import Control.Monad.State.Strict
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (Maybe)
import qualified Data.Maybe as Maybe

-- NOTE:
-- We use specific functions instead of a custom instance of Show so that
-- we can clearly see the output of different stages of the pipeline when
-- printed to the console, rather than the eventual code generated

type Code = String

header = "import Control.Parallel\nimport Control.Parallel.Strategies"

encode :: EncodingInstructionTable -> Code
encode 
    = intercalate "\n\n" . (:) header . map encode' . Map.toList
    where
        typeSignature = Maybe.maybe "" ((++) "\n" . signatureToCode)
        definition    = (flip (++) " = " .) . (unwords .) . (:)
        encode' :: (Name, EncodingInstruction) -> Code
        encode' (name, Sequential mts params e)
            = typeSignature mts 
                ++ definition name params 
                ++ exprToCode e
        encode' (name, Parallel mts params dg)
            = typeSignature mts 
                ++ definition name params 
                ++ graphToCode dg True

--- Expr Code Generation ---

litToCode :: Lit -> Code
litToCode (LInt i)
    = show i
litToCode (LBool b)
    = show b
litToCode (LList ls)
    = "[" ++ intercalate ", " (map exprToCode ls) ++ "]"

opToCode :: BinOp -> Code
opToCode Add  = "+"
opToCode Sub  = "-"
opToCode Mul  = "*" 
opToCode Div  = "/"
opToCode Exp  = "^"
opToCode EQ   = "=="
opToCode LT   = "<"
opToCode GT   = ">"
opToCode LTE  = "<="
opToCode GTE  = ">="
opToCode And  = "&&"
opToCode Or   = "||"
opToCode Cons = ":"

defToCode :: Def -> Code
defToCode (Def n e)
    = n ++ " = " ++ exprToCode e

parenthesisedExprToCode :: Expr -> Code
parenthesisedExprToCode l @ Lit{}
    = exprToCode l
parenthesisedExprToCode v @ Var{}
    = exprToCode v
parenthesisedExprToCode e
    = "(" ++ exprToCode e ++ ")"

exprToCode :: Expr -> Code
exprToCode (Lit l)
    = litToCode l
exprToCode (Var n) 
    = n
exprToCode (App e1 e2)
    = exprToCode e1 ++ " " 
        ++ parenthesisedExprToCode e2
exprToCode (Op op e1 e2)
    = parenthesisedExprToCode e1 
        ++ " " ++ opToCode op ++ " "
        ++ parenthesisedExprToCode e2
exprToCode (Let ds e)
    = "let " 
        ++ intercalate "; " (map defToCode ds) 
        ++ " in " ++ exprToCode e
exprToCode (If e1 e2 e3)
    = "if " ++ exprToCode e1
        ++ " then " ++ exprToCode e2
        ++ " else " ++ exprToCode e3

--- Type Code Generation ---

typeToCode :: Type -> Code
typeToCode Int      = "Int"
typeToCode Bool     = "Bool"
typeToCode (List t) = "[" ++ typeToCode t ++ "]"

signatureToCode :: [Type] -> Code
signatureToCode
    = intercalate " -> " . map typeToCode

--- Dependency Graph Generation ---

dlitToCode :: DLit -> Code
dlitToCode (DInt i)
    = show i
dlitToCode (DBool b)
    = show b
dlitToCode (DList ls)
    = "[" ++ intercalate ", " (map dexprToCode ls) ++ "]"

parenthesisedDExprToCode :: DExpr -> Code
parenthesisedDExprToCode l @ DLit{}
    = dexprToCode l
parenthesisedDExprToCode v @ DVar{}
    = dexprToCode v
parenthesisedDExprToCode e
    = "(" ++ dexprToCode e ++ ")"


dexprToCode :: DExpr -> Code
dexprToCode (DLit l)
    = dlitToCode l
dexprToCode (DVar n)
    = n
dexprToCode (DApp n es)
    = unwords (n : map parenthesisedDExprToCode es)
dexprToCode (DOp op e1 e2)
    = parenthesisedDExprToCode e1 
        ++ " " ++ opToCode op ++ " "
        ++ parenthesisedDExprToCode e2

type GeneratedNames = Set Name
type ParallelisedScope = Bool
type GenerationState = (DependencyGraph, GeneratedNames, ParallelisedScope)

-- getChildren returns a set of all children of the given node, except those
-- with conditional dependencies.
getChildren :: Name -> State GenerationState (Set Name)
getChildren n = do
    ((_, ds), _, _) <- get
    return $ 
        Set.map (\(_, c, _) -> c) $
        Set.filter (\(p, _, t) -> p == n && t == Dep) ds


-- getParents returns a set of all parents of the given node, except those
-- with conditional dependencies.
getParents :: Name -> State GenerationState (Set Name)
getParents n = do
    ((_, ds), _, _) <- get
    return $ 
        Set.map (\(p, _, _) -> p) $
        Set.filter (\(_, c, t) -> c == n && t == Dep) ds


-- getBranch finds the name of the node which is a branch of the given node
-- and has the given dependency type. Used for finding the conditional
-- dependencies, and not for regular dependencies.
getBranch :: Name -> DType -> State GenerationState Name
getBranch p t = do
    ((_, ds), _, _) <- get
    let bs = Set.filter (\(p', _, t') -> p == p' && t == t') ds
    return $ ((\(_, c, _) -> c) . Set.elemAt 0) bs


-- satisifiedChildren returns the children of the given node whose 
-- dependencies are met, i.e. their code has been generated.
satisfiedChildren :: Name -> State GenerationState [Name]
satisfiedChildren name = do
    (_, gns, _) <- get
    children <- Set.toList <$> getChildren name
    cps <- zip children <$> mapM getParents children
    return [c | (c, ps) <- cps, Set.isSubsetOf ps gns]


-- getNode retrieves the node with the given name.
getNode :: Name -> State GenerationState DNode
getNode name = do
    ((ns, _), _, _) <- get
    return (ns Map.! name)


-- updateGeneratedNames records that a node with the given name has had its
-- code generated.
updateGeneratedNames :: Name -> State GenerationState ()
updateGeneratedNames gn = do
    (g, gns, par) <- get
    put (g, Set.insert gn gns, par)


-- graphToCode generates code from the given dependency graph,
-- either encoding it sequentially if False, else parallelised.
graphToCode :: DependencyGraph -> Bool -> Code
graphToCode g False
    = snd $ evalState (graphToSequentialCode "_") (g, Set.empty, False)
graphToCode g True
    = snd $ evalState (graphToParallelCode "_") (g, Set.empty, False)


graphToSequentialCode :: Name -> State GenerationState (Name, Code)
graphToSequentialCode name = do
    updateGeneratedNames name
    node <- getNode name
    -- generate the code for the children of the current node, if all other
    -- dependencies for that child have already been generated
    let generateChildren = do
            (lns, cs) <- unzip <$> (satisfiedChildren name >>= mapM graphToSequentialCode)
            let mLastName = if null lns then Nothing else Just (last lns)
            return (mLastName, intercalate "; " cs)
    case node of
        Scope -> do
            -- Scope is encoded as `let {children} in {final descendent}`
            (mLastName, code) <- generateChildren
            let lastName = Maybe.fromJust mLastName
            return (lastName, "let " ++ code ++ " in " ++ lastName)
        Expression e -> do
            -- Expression is encoded as `{name} = {e} {; children}`
            (mLastName, code) <- generateChildren
            let expCode = name ++ " = " ++ dexprToCode e
            case mLastName of
                Just lastName -> return (lastName, expCode ++ "; " ++ code)
                Nothing       -> return (name, expCode)
        Conditional e -> do
            -- Conditional is encoded as 
            -- `{name} = if {e} then {then code} else {else code} ; {children}`
            (_, thenCode) <- getBranch name DepThen >>= graphToSequentialCode
            (_, elseCode) <- getBranch name DepElse >>= graphToSequentialCode
            (mLastName, code) <- generateChildren
            let endCode = name ++ " = if " ++ dexprToCode e
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
scopeContainsParallelism :: Name -> State GenerationState Bool
scopeContainsParallelism start =
    or <$> (Set.toList <$> getChildren start >>= mapM scopeContainsParallelism')
    where 
        scopeContainsParallelism' :: Name -> State GenerationState Bool
        scopeContainsParallelism' name = do
            node <- getNode name
            case node of
                Scope              -> return False
                Expression DApp{}  -> return True
                Conditional DApp{} -> return True
                _                  ->
                    or <$> (Set.toList <$> getChildren name >>= mapM scopeContainsParallelism')

    
graphToParallelCode :: Name -> State GenerationState (Name, Code)
graphToParallelCode name = do
    updateGeneratedNames name
    node <- getNode name
    let generateChildren = do
            (lns, cs) <- unzip <$> (satisfiedChildren name >>= mapM graphToParallelCode)
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
                    (_, DApp{}) -> 
                        name ++ " <- rpar (" ++ dexprToCode e ++ ")"
                    (True, _)        ->
                        "let { " ++ name ++ " = " ++ dexprToCode e ++ " }"
                    _                ->
                        name ++ " = " ++ show e
            case mLastName of
                Just lastName -> return (lastName, expCode ++ "; " ++ code)
                Nothing       -> return (name, expCode)
        Conditional e -> do
            (_, thenCode) <- getBranch name DepThen >>= graphToParallelCode
            (_, elseCode) <- getBranch name DepElse >>= graphToParallelCode
            (mLastName, code) <- generateChildren
            par <- getParallelisedScope
            let condCode = name ++ " = if " ++ dexprToCode e 
                            ++ " then " ++ thenCode 
                            ++ " else " ++ elseCode 
            let endCode = if par then "let { " ++ condCode ++ " }" else condCode
            return (Maybe.fromMaybe name mLastName, endCode ++ "; " ++ code)
