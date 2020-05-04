module Backend.CodeGenerator where

import Hashkell.Syntax
import Middleend 
    ( DependencyGraph
    , Dependency
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
-- We use specific functions instead of a custom instance of Show so that we
-- can clearly see the output of different stages of the pipeline when printed
-- to the console when testing functions individually, rather than the eventual
-- code generated

type Code = String

-- header gives the imports for the parallelisation functions
header = "import Control.Parallel\nimport Control.Parallel.Strategies"

-- encode translates each encoding instruction into code
encode :: EncodingInstructionTable -> Code
encode 
    = intercalate "\n\n" . (:) header . map encode' . Map.toList
    where
        -- encoding the type signature
        typeSignature n = Maybe.maybe "" 
                            ((++) (n ++ " :: ") 
                            . flip (++) "\n" 
                            . signatureToCode)
        -- encoding the definition signature
        definition    = (flip (++) " = " .) . (unwords .) . (:)
        encode' :: (Name, EncodingInstruction) -> Code
        encode' (name, Sequential mts params e)
            = typeSignature name mts 
                ++ definition name params 
                ++ exprToCode e
        encode' (name, Parallel mts params dg)
            = typeSignature name mts 
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

-- (aids readability in the final code by minimising the amount of
-- parenthesises around each expression, while ensuring correctness)
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
dexprToCode (DAtomApp n es)
    = unwords (n : map parenthesisedDExprToCode es)
dexprToCode (DHighApp n f es)
    = unwords (n : map parenthesisedDExprToCode (f : es))
dexprToCode (DOp op e1 e2)
    = parenthesisedDExprToCode e1 
        ++ " " ++ opToCode op ++ " "
        ++ parenthesisedDExprToCode e2

type GeneratedNames = Set Name
type ParallelisedScope = Bool

data GenerationState 
    = BasicState DependencyGraph GeneratedNames
    | ScopedState DependencyGraph GeneratedNames ParallelisedScope

getDependencies :: State GenerationState (Set Dependency)
getDependencies = do
    s <- get
    return $ case s of
        BasicState (_, ds) _ -> ds
        ScopedState (_, ds) _ _ -> ds

getGeneratedNames :: State GenerationState GeneratedNames
getGeneratedNames = do
    s <- get
    return $ case s of
        BasicState _ gn -> gn
        ScopedState _ gn _ -> gn

getNodeTable :: State GenerationState (Map Name DNode)
getNodeTable = do
    s <- get
    return $ case s of
        BasicState (ns, _) _ -> ns
        ScopedState (ns, _) _ _ -> ns

-- getChildren returns a set of all children of the given node, except those
-- with conditional dependencies.
getChildren :: Name -> State GenerationState (Set Name)
getChildren n =
    Set.map (\(_, c, _) -> c) .
        Set.filter (\(p, _, t) -> p == n && t == Dep) <$> getDependencies


-- getParents returns a set of all parents of the given node, except those
-- with conditional dependencies.
getParents :: Name -> State GenerationState (Set Name)
getParents n =
    Set.map (\(p, _, _) -> p) .
        Set.filter (\(_, c, t) -> c == n && t == Dep) <$> getDependencies


-- getBranch finds the name of the node which is a branch of the given node
-- and has the given dependency type. Used for finding the conditional
-- dependencies, and not for regular dependencies.
getBranch :: Name -> DType -> State GenerationState Name
getBranch p t =
    ((\(_, c, _) -> c) . Set.elemAt 0) .
        Set.filter (\(p', _, t') -> p == p' && t == t') <$> getDependencies


-- satisifiedChildren returns the children of the given node whose 
-- dependencies are met, i.e. their code has been generated.
satisfiedChildren :: Name -> State GenerationState [Name]
satisfiedChildren name = do
    gns <- getGeneratedNames
    children <- Set.toList <$> getChildren name
    cps <- zip children <$> mapM getParents children
    return [c | (c, ps) <- cps, Set.isSubsetOf ps gns]


-- getNode retrieves the node with the given name.
getNode :: Name -> State GenerationState DNode
getNode name =
    flip (Map.!) name <$> getNodeTable


-- updateGeneratedNames records that a node with the given name has had its
-- code generated.
updateGeneratedNames :: Name -> State GenerationState ()
updateGeneratedNames gn = do
    s <- get
    case s of
        BasicState dg gns 
            -> put (BasicState dg (Set.insert gn gns))
        ScopedState dg gns par
            -> put (ScopedState dg (Set.insert gn gns) par)


-- graphToCode generates code from the given dependency graph,
-- either encoding it sequentially if False, else parallelised.
graphToCode :: DependencyGraph -> Bool -> Code
graphToCode g False
    = snd $ evalState (graphToSequentialCode "_") 
        (BasicState g Set.empty)
graphToCode g True
{-
    = snd $ evalState (graphToParallelCodeAll "_")
        (BasicState g Set.empty)
-}
    = snd $ evalState (graphToParallelCodeCalls "_")
        (ScopedState g Set.empty False)
{-
    = code
    where (_, code, _) = evalState (graphToParallelCodePathed "_") (ScopedState g Set.empty False)
-}


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
    s <- get
    case s of
        ScopedState dg gns par' ->
            put (ScopedState dg gns par) >> return par'

-- getParallelisedScope retrieves the bool determining whether the current code
-- in the current scope in the graph should be parallelised.
getParallelisedScope :: State GenerationState Bool
getParallelisedScope = do
    s <- get
    case s of
        ScopedState _ _ par -> return par


-- scopeContainsParallelism returns whether the current scope in the graph
-- contains parallel branches and non-atomic function calls, representing the
-- potential for parallelism
scopeContainsParallelism :: Name -> State GenerationState Bool
scopeContainsParallelism start
    = (||) <$> hasHigherOrderFunctionCalls start <*> ((&&) <$> hasBranches start <*> hasFunctionCalls start)
    where
        -- NOTE: recall that getChildren only considers Dep dependencies
        -- check if this scope has branches
        hasBranches :: Name -> State GenerationState Bool
        hasBranches n = do
            children <- getChildren n
            if Set.null children
                then return False
                else if Set.size children > 1
                    then return True
                    else (hasBranches . head . Set.toList) children
        -- check if this scope has function calls
        hasFunctionCalls :: Name -> State GenerationState Bool
        hasFunctionCalls n = do
            node <- getNode n
            let checkChildren = or <$> 
                    (Set.toList <$> getChildren n >>= mapM hasFunctionCalls)
            case node of
                Expression DApp{} -> return True
                _                 -> checkChildren
        hasHigherOrderFunctionCalls :: Name -> State GenerationState Bool
        hasHigherOrderFunctionCalls n = do
            node <- getNode n
            let checkChildren = or <$> 
                    (Set.toList <$> getChildren n 
                        >>= mapM hasHigherOrderFunctionCalls)
            case node of
                Expression (DHighApp _ DAtomApp{} _) -> checkChildren
                Expression DHighApp{}                -> return True
                _                                    -> checkChildren

graphToParallelCodeAll :: Name -> State GenerationState (Name, Code)
graphToParallelCodeAll name = do
    updateGeneratedNames name
    node <- getNode name
    let generateChildren = do
            (lns, cs) <- unzip <$> (satisfiedChildren name >>= mapM graphToParallelCodeAll)
            let lastName = if null lns then name else last lns
            return (lastName, intercalate "; " cs)
    case node of
        Scope -> do
            (lastName, code) <- generateChildren
            let finalCode = "runEval $ do { " ++ code ++ "; return " ++  lastName ++ "}"
            return (lastName, finalCode)
        Expression e -> do
            (lastName, code) <- generateChildren
            let expCode = case e of
                    DHighApp _ DApp{} _ ->
                        name ++ " <- parList rdeepseq " ++ parenthesisedDExprToCode e
                    _ ->
                        name ++ " <- rpar " ++ parenthesisedDExprToCode e
            if null code
                then return (lastName, expCode)
                else return (lastName, expCode ++ "; " ++ code)
        Conditional e -> do
            (_, thenCode) <- getBranch name DepThen >>= graphToParallelCodeAll
            (_, elseCode) <- getBranch name DepElse >>= graphToParallelCodeAll
            (lastName, code) <- generateChildren
            let condCode = name ++ " <- rpar (if " ++ dexprToCode e 
                            ++ " then " ++ thenCode 
                            ++ " else " ++ elseCode  ++ ")"
                endCode = if null code then condCode else condCode ++ "; " ++ code
            return (lastName, endCode)

    
graphToParallelCodeCalls :: Name -> State GenerationState (Name, Code)
graphToParallelCodeCalls name = do
    updateGeneratedNames name
    node <- getNode name
    let generateChildren = do
            (lns, cs) <- unzip <$> (satisfiedChildren name >>= mapM graphToParallelCodeCalls)
            let lastName = if null lns then name else last lns
            return (lastName, intercalate "; " cs)
    case node of
        Scope -> do
            par <- scopeContainsParallelism name
            oldPar <- setParallelisedScope par
            (lastName, code) <- generateChildren
            setParallelisedScope oldPar
            let finalCode = if par
                then "runEval $ do { " ++ code ++ "; return " ++  lastName ++ "}"
                else "let " ++ code ++ " in " ++ lastName
            return (lastName, finalCode)
        Expression e -> do
            (lastName, code) <- generateChildren
            par <- getParallelisedScope
            let expCode = if par
                    then case e of
                        DApp{} ->
                            name ++ " <- rpar " ++ parenthesisedDExprToCode e
                        DHighApp _ DApp{} _ ->
                            name ++ " <- parList rdeepseq " ++ parenthesisedDExprToCode e
                        _ ->
                            "let { " ++ name ++ " = " ++ dexprToCode e ++ " }"
                    else name ++ " = " ++ dexprToCode e
            if null code
                then return (lastName, expCode)
                else return (lastName, expCode ++ "; " ++ code)
        Conditional e -> do
            (_, thenCode) <- getBranch name DepThen >>= graphToParallelCodeCalls
            (_, elseCode) <- getBranch name DepElse >>= graphToParallelCodeCalls
            (lastName, code) <- generateChildren
            par <- getParallelisedScope
            let condCode = name ++ " = if " ++ dexprToCode e 
                            ++ " then " ++ thenCode 
                            ++ " else " ++ elseCode 
            let endCode = if par then "let { " ++ condCode ++ " }" else condCode
            return (lastName, endCode ++ "; " ++ code)

graphToParallelCodePathed :: Name -> State GenerationState (Name, Code, Bool)
graphToParallelCodePathed name = do
    updateGeneratedNames name
    node <- getNode name
    let generateChildren = do
            satisfied <- satisfiedChildren name
            numChildren <- Set.size <$> getChildren name
            (lns, cs, ps) <- unzip3 <$> mapM graphToParallelCodePathed satisfied
            let lastName = if null lns then name else last lns
                -- a node is in an alternative path if it has unsatisfied
                -- children, or the last child traversed was part of an
                -- alternative path
                isAltPath = (numChildren /= 0) && ((numChildren > length satisfied) || last ps)
            return (lastName, intercalate "; " cs, isAltPath)
    case node of
        Scope -> do
            par <- scopeContainsParallelism name
            oldPar <- setParallelisedScope par
            (lastName, code, _) <- generateChildren
            setParallelisedScope oldPar
            let finalCode = if par
                then "runEval $ do { " ++ code ++ "; return " ++  lastName ++ "}"
                else "let " ++ code ++ " in " ++ lastName
            return ("", finalCode, False)
        Expression e -> do
            (lastName, code, isAltPath) <- generateChildren
            par <- getParallelisedScope
            lenChildren <- Set.size <$> getChildren name
            let expCode = 
                    case (par, isAltPath, e) of
                        -- always parallelise higher order function if in
                        -- parallelised scope
                        (True, _, DHighApp _ DApp{} _) ->
                            name ++ " <- parList rdeepseq " ++ parenthesisedDExprToCode e
                        -- parallelise function calls if in parallelised scope
                        -- and alternative path
                        (True, True, DApp{}) ->
                            name ++ " <- rpar " ++ parenthesisedDExprToCode e
                        -- do not parallelise if in a parallelised scope but
                        -- in the main path
                        (True, _, _) ->
                            "let { " ++ name ++ " = " ++ dexprToCode e ++ " }"
                        (False, _, _) ->
                            name ++ " = " ++ dexprToCode e
            if null code
                then return (lastName, expCode, isAltPath)
                else return (lastName, expCode ++ "; " ++ code, isAltPath)
        Conditional e -> do
            (_, thenCode, _) <- getBranch name DepThen >>= graphToParallelCodePathed
            (_, elseCode, _) <- getBranch name DepElse >>= graphToParallelCodePathed
            (lastName, code, isAltPath) <- generateChildren
            par <- getParallelisedScope
            let condCode = name ++ " = if " ++ dexprToCode e 
                            ++ " then " ++ thenCode 
                            ++ " else " ++ elseCode 
            let endCode = if par then "let { " ++ condCode ++ " }" else condCode
            return (lastName, endCode ++ "; " ++ code, isAltPath)