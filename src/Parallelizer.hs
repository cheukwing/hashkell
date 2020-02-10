module Parallelizer where

import Simple.Syntax

import Prelude hiding (EQ, GT, LT)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type FunctionDefn = Expr
type FunctionCplx = Expr
-- TODO: consider using maybes for defn and cplx
type FunctionData = ([Name], FunctionDefn, FunctionCplx)
type FunctionTable = Map.Map Name FunctionData

type DName = String
type DTable = Map.Map DName DNode

buildFunctionTable :: Prog -> FunctionTable
buildFunctionTable 
    = foldl buildFunctionTable' Map.empty
    where
        buildFunctionTable' :: FunctionTable -> Decl -> FunctionTable
        buildFunctionTable' ft (Func name args expr)
            | isMember && fvsValid  = Map.insert name (args, expr, cplx) ft
            | otherwise             = Map.insert name (args, expr, Lit (LInt 1)) ft
            where
                isMember     = Map.member name ft
                (_, _, cplx) = (Map.!) ft name
                fvsValid     = Set.foldl (\t v -> ((&&) t . flip elem args) v) True (freeVariables cplx)
        buildFunctionTable' ft (Cplx name expr)
            | isValid && isMember && fvsValid = Map.insert name (args, defn, expr) ft
            | isValid && not isMember         = Map.insert name ([], Lit (LInt 1), expr) ft
            | otherwise                       = ft
            where
                isValid         = isValidComplexity expr
                isMember        = Map.member name ft
                (args, defn, _) = (Map.!) ft name
                fvsValid        = Set.foldl (\t v -> ((&&) t . flip elem args) v) True (freeVariables expr)


isValidComplexity :: Expr -> Bool
isValidComplexity If{}           = False
isValidComplexity Let{}          = False
-- TODO: support log application
isValidComplexity App{}          = False
isValidComplexity Var{}          = True
isValidComplexity (Lit (LInt _)) = True
isValidComplexity Lit{}          = False
isValidComplexity (Op op e1 e2)
    = op `elem` [Add, Sub, Mul, Div, Exp] 
        && isValidComplexity e1 
        && isValidComplexity e2


freeVariables :: Expr -> Set.Set Name
freeVariables (If e1 e2 e3)
    = Set.unions [freeVariables e1, freeVariables e2, freeVariables e3]
freeVariables (Let defs e)
    = Set.difference (freeVariables e) bound
    where
        bound = Set.fromList $ map (\(Def name _) -> name) defs
freeVariables (App e1 e2)
    = Set.union (freeVariables e1) (freeVariables e2)
freeVariables (Var n)
    = Set.singleton n
freeVariables Lit{}
    = Set.empty
freeVariables (Op _ e1 e2)
    = Set.union (freeVariables e1) (freeVariables e2)

splitFunctions :: FunctionTable -> FunctionTable
splitFunctions 
    = Map.foldlWithKey splitFunction Map.empty
    where
        splitFunction :: FunctionTable -> Name -> FunctionData -> FunctionTable
        splitFunction st name fd @ (_ , _, Lit{})
            = Map.insert name fd st
        splitFunction st name (args, defn, cplx)
            = st'
            where
                st'            = Map.union (Map.fromList [(name, start), (name ++ "_seq", (args, defn, Lit (LInt 1)))]) st
                callFunction n = foldl (\app a -> App app (Var a)) (Var n) args 
                start          = (args, If (Op LT cplx (Lit (LInt 100)))
                                                (callFunction $ name ++ "_seq")
                                                (callFunction $ name ++ "_par")
                                 , cplx)
            

data DNode
    = DBranch [DName]
    | DArg [DName]
    | DIf DName [DName] DName DName
    | DDep DName DExpr [DName]
    deriving (Eq, Show)

data DExpr
    = DApp Name [DExpr]
    | DOp BinOp DExpr DExpr
    | DVar Name
    | DLit Lit
    deriving (Eq, Show)

depName :: Int -> String
depName n = "_x" ++ show n

addDependent :: DNode -> DName -> DNode
addDependent (DBranch deps) dep
    = DBranch (dep : deps)
addDependent (DArg deps) dep
    = DArg (dep : deps)
addDependent (DIf name deps thenBranch elseBranch) dep
    = DIf name (dep : deps) thenBranch elseBranch
addDependent (DDep name exp deps) dep
    = DDep name exp (dep : deps)
        
addDependentToNode :: DTable -> DName -> DName -> DTable
addDependentToNode dt name dep
    = Map.insert name (addDependent ((Map.!) dt name) dep) dt


createDTable :: Expr -> DTable
createDTable = snd . toDTable (Map.fromList [("_", DBranch [])]) "_" 0

toDTable :: DTable -> DName -> Int -> Expr -> (Int, DTable)
toDTable dt base i (Lit lit)
    = (i + 1, dt'')
    where 
        name = depName i
        node = DDep name (DLit lit) []
        dt'  = addDependentToNode dt base name
        dt'' = Map.insert name node dt'
toDTable dt base i (Var var)
    = (i + 1, dt'')
    where
        name = depName i
        node = DDep name (DVar var) []
        dt'  = addDependentToNode dt var name
        dt'' = Map.insert name node dt'
toDTable dt base i (Op op e1 e2)
    = (i'' + 1, dt''''')
    where
        -- setup graphs for subexpressions
        (i', dt')   = toDTable dt base i e1
        (i'', dt'') = toDTable dt' base i' e2
        nameLeft    = depName (i' - 1)
        nameRight   = depName (i'' - 1)
        name        = depName i''
        -- add self as dependent
        dt'''       = addDependentToNode dt'' nameLeft name
        dt''''      = addDependentToNode dt''' nameRight name
        -- add self to table
        node        = DDep name (DOp op (DVar nameLeft) (DVar nameRight)) []
        dt'''''     = Map.insert name node dt''''
toDTable dt base i e @ App{}
    = (i'' + 1, dt'''')
    where
        -- assume full applications
        -- setup graphs for all args
        (appName, args) = appArgs e
        (is, dts)       = unzip $ scanl (\(i', dt') a -> toDTable dt' base i' a) (i, dt) args
        depNames        = map (depName . flip (-) 1) is
        i''             = last is
        dt''            = last dts
        name            = depName i''
        -- add self as dependent to all args
        dt'''           = foldl (\dt' dn -> addDependentToNode dt' dn name) dt'' depNames
        -- add self to table
        node            = DDep name (DApp appName (map DVar depNames)) []
        dt''''          = Map.insert name node dt'''
        appArgs :: Expr -> (Name, [Expr])  
        appArgs (App (Var name) e)
          = (name, [e])
        appArgs (App e1 e2)
          = (name, e2 : es)
          where (name, es) = appArgs e1
toDTable dt base i (Let defs e)
    = (i'' + 1, dt''''')
    where
        -- setup graphs for all defs
        (i', dt')  = foldl defToDGraph (i, dt) defs
        -- setup graph for expression
        (i'', dt'') = toDTable dt' base i' e
        expName     = depName (i'' - 1)
        name        = depName i'' 
        -- add self as dependent to all defs and expression
        dt'''       = foldl (\dtable (Def defName _) -> addDependentToNode dtable defName name) dt'' defs
        dt''''      = addDependentToNode dt''' expName name
        -- add self to table
        node        = DDep name (DVar expName) []
        dt'''''     = Map.insert name node dt''''
        defToDGraph :: (Int, DTable) -> Def -> (Int, DTable)
        defToDGraph (i, dt) (Def name e)
            = (i', dt''')
            where
                (i', dt') = toDTable dt base i e
                nameSub   = depName (i' - 1)
                dt''      = addDependentToNode dt' nameSub name
                node      = DDep name (DVar nameSub) []
                dt'''     = Map.insert name node dt''
toDTable dt base i (If e1 e2 e3)
    = (i''' + 1, dt'''''')
    where
        -- setup graph for condition
        (i', dt')      = toDTable dt base i e1
        condName       = depName (i' - 1)
        -- setup branch nodes
        thenBranch     = DBranch []
        thenName       = "_" ++ show i'
        elseBranch     = DBranch []
        elseName       = "_" ++ show (i' + 1)
        dt''           = Map.insert elseName elseBranch (Map.insert thenName thenBranch dt')
        -- set then and else expressions to branch off from branch nodes
        (i'', dt''')   = toDTable dt'' thenName (i' + 2) e2
        (i''', dt'''') = toDTable dt''' elseName i'' e3
        name           = depName i'''
        -- add self as dependent to condition
        dt'''''        = addDependentToNode dt'''' condName name
        -- set then and else branches as dependents
        node           = DIf name [] thenName elseName
        -- add self to to table
        dt''''''       = Map.insert name node dt'''''


{-
isAtomic :: FunctionTable -> Expr -> Bool
isAtomic _ Lit{} 
    = True
isAtomic ft (Var name) 
    = not $ Map.member name ft
isAtomic ft (Op _ e1 e2)
    = isAtomic ft e1 && isAtomic ft e2
isAtomic ft (If e1 e2 e3)
    = isAtomic ft e1 && isAtomic ft e2 && isAtomic ft e3
isAtomic ft (Let defs e)
    = all (map (\Def _ def -> isAtomic ft def) defs) && isAtomic ft e
isAtomic _ App{}
    = False

toDExpr :: Expr -> DExpr
toDExpr (Var name)
    = DVar name
toDExpr (Lit lit)
    = DLit lit
toDExpr (Op op e1 e2)
    = DOp op (toDExpr e1) (toDExpr e2)
toDExpr (App (Var name) e2)
    = DApp name [toDExpr e2]
toDExpr (App e1 e2)
    = DApp name (toDExpr e2 : es)
    where DApp name es = toDExpr e1

-}