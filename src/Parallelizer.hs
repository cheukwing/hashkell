module Parallelizer (
    createFunctionTable,
    FunctionTable,
    FunctionData(..)
) where

import Simple.Syntax

import Prelude hiding (EQ, GT, LT)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Either (Either)
import qualified Data.Either as Either
import DependencyGraph

type FunctionDefn = Expr

-- TODO: support factorial
data Complexity
    = Constant Int
    | Polynomial Name Int
    | Exponential Int Name
    | Logarithmic Name


data InitFunctionData 
    = JustAnnotation Expr
    | JustDefinition [Name] FunctionDefn
    | ComplexityAndDefinition [Name] FunctionDefn Complexity
type InitFunctionTable = Map Name InitFunctionData

type FunctionTable = Map Name FunctionData
data FunctionData
    = Sequential [Name] FunctionDefn
    | Parallel [Name] DependencyGraph
    deriving Show


createFunctionTable :: Prog -> Int -> FunctionTable
createFunctionTable prog steps
    = buildFunctionTable steps (buildInitFunctionTable prog)

-- buildInitFunctionTable builds the initial function table by aggregating
-- and checking compatibility of each function's parsed declarations.
buildInitFunctionTable :: Prog -> InitFunctionTable
buildInitFunctionTable 
    = foldl buildFunctionTable' Map.empty
    where
        buildFunctionTable' :: InitFunctionTable -> Decl -> InitFunctionTable
        buildFunctionTable' ft (Func name args defn)
            = case Map.lookup name ft of
                Just (JustAnnotation e) -> 
                    Map.insert name (ComplexityAndDefinition args defn (parseComplexity args e)) ft
                Just _                  ->
                    error $ "Duplicate definition of function " ++ name
                Nothing                 ->
                    Map.insert name (JustDefinition args defn) ft
        buildFunctionTable' ft (Cplx name e)
            = case Map.lookup name ft of
                Just (JustDefinition args defn) -> 
                    Map.insert name (ComplexityAndDefinition args defn (parseComplexity args e)) ft
                Just _                  ->
                    error $ "Duplicate complexity annotation for function " ++ name
                Nothing                 ->
                    Map.insert name (JustAnnotation e) ft


-- buildFunctionTable uses the initial function table to create the final
-- function table, containing the information and which functions to generate,
-- and how they should be generated.
buildFunctionTable :: Int -> InitFunctionTable -> FunctionTable
buildFunctionTable steps
    = Map.foldlWithKey splitFunction Map.empty
    where
        splitFunction :: FunctionTable -> Name -> InitFunctionData -> FunctionTable
        splitFunction ft name JustAnnotation{}
            = ft
        splitFunction ft name (JustDefinition args defn)
            = Map.insert name (Sequential args defn) ft
        splitFunction ft name (ComplexityAndDefinition args defn cplx)
            = case complexityToBoundary cplx steps of
                Left False     -> 
                    Map.insert name sequential ft
                Left True      ->
                    Map.insert name parallel ft
                Right cplxExpr ->
                    Map.union (Map.fromList [ (name, Sequential args branchingCall)
                                            , (seqName, sequential)
                                            , (parName, parallel)
                                            ]) ft
                    where
                        seqName        = name ++ "_seq"
                        parName        = name ++ "_par"
                        callFunction n = foldl (\app a -> App app (Var a)) (Var n) args 
                        branchingCall  = If cplxExpr (callFunction seqName) (callFunction parName)
            where
                sequential = Sequential args defn
                parallel   = Parallel args (createDependencyGraph args defn)


--- HELPER FUNCTIONS ---

-- parseComplexity returns the complexity of an annotation if its expressions
-- are supported and variables compatible, else throws an error
parseComplexity :: [Name] -> Expr -> Complexity
parseComplexity args expr
    | fvsValid && sizeValid = parseComplexityExpression expr
    | not fvsValid          = error errorFvsMsg
    | otherwise             = error errorSizeMsg
    where
        fvs          = freeVariables expr
        fvsValid     = Set.isSubsetOf fvs (Set.fromList args)
        sizeValid    = Set.size fvs <= 1
        errorFvsMsg  = "The time complexity annotation " ++ show expr ++ " is incompatible with the function definition"
        errorSizeMsg = "The time complexity annotation " ++ show expr ++ " is unsupported as it contains more than 1 name"


-- parseComplexityExpression parses the annotation and returns its associated
-- complexity if it is supported, else throws an error
parseComplexityExpression :: Expr -> Complexity
parseComplexityExpression If{} 
    = error "Cannot use 'if' in a time complexity annotation"
parseComplexityExpression Let{} 
    = error "Cannot use 'let' in a time complexity annotation"
parseComplexityExpression (App (Var "log") (Var name)) 
    = Logarithmic name
parseComplexityExpression (App (Var "log") _) 
    = error "Only logarithmic complexity with a single name is supported"
parseComplexityExpression App{}
    = error "Cannot use arbitrary function application in a time complexity annotation"
parseComplexityExpression (Var name)
    = Polynomial name 1
parseComplexityExpression (Lit (LInt n))
    = Constant n
parseComplexityExpression Lit{}
    = error "Cannot use arbitrary literal in a time complexity annotation"
parseComplexityExpression (Op Exp (Var name) (Lit (LInt n)))
    = Polynomial name n
parseComplexityExpression (Op Exp (Lit (LInt n)) (Var name))
    = Exponential n name
parseComplexityExpression Op{}
    = error "Only polynomials and exponentials with a single name are supported"


-- freeVariables returns all the free variables in a given expression
freeVariables :: Expr -> Set Name
freeVariables (If e1 e2 e3)
    = Set.unions [freeVariables e1, freeVariables e2, freeVariables e3]
freeVariables (Let defs e)
    = Set.difference (freeVariables e) bound
    where bound = Set.fromList $ map (\(Def name _) -> name) defs
freeVariables (App e1 e2)
    = Set.union (freeVariables e1) (freeVariables e2)
freeVariables (Var n)
    = Set.singleton n
freeVariables Lit{}
    = Set.empty
freeVariables (Op _ e1 e2)
    = Set.union (freeVariables e1) (freeVariables e2)


-- complexityToBoundary takes a complexity and the step approximation to
-- determine the boundary. It will return either a bool signifying whether or
-- not to parallelise, or an expression to use as the condition for conditional
-- parallelisation.
complexityToBoundary :: Complexity -> Int -> Either Bool Expr
complexityToBoundary (Constant n) steps
    = Left (n > steps)
complexityToBoundary (Polynomial name n) steps
    = Right (Op LT (Var name) (Lit (LInt (ceiling $ fromIntegral steps ** (1 / fromIntegral n)))))
complexityToBoundary (Exponential n name) steps
    = Right (Op LT (Var name) (Lit (LInt (ceiling $ logBase (fromIntegral n) (fromIntegral steps)))))
complexityToBoundary (Logarithmic name) steps
    = Right (Op LT (Var name) (Lit (LInt (2 ^ steps))))