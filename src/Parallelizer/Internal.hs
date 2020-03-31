module Parallelizer.Internal where

import Simple.Syntax

import Prelude hiding (EQ, GT, LT)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Either (Either)
import qualified Data.Either as Either
import DependencyGraph

type Defn = Expr
type Cplx = Expr

-- TODO: support factorial
data Complexity
    = Constant Int
    | Polynomial Name Int
    | Exponential Int Name
    | Logarithmic Name
    deriving (Eq, Show)


data InitFunctionData 
    = Complexity Cplx
    | TypeAnnotation [Type]
    | Definition [Name] Defn
    | ComplexityType Cplx [Type]
    | ComplexityDefinition Cplx [Name] Defn
    | TypeDefinition [Type] [Name] Defn
    | Complete Cplx [Type] [Name] Defn
type InitFunctionTable = Map Name InitFunctionData

type FunctionTable = Map Name FunctionData
data FunctionData
    = Sequential [Name] Defn
    | SequentialT [Type] [Name] Defn
    | Parallel [Name] DependencyGraph
    | ParallelT [Type] [Name] DependencyGraph
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
                Just (Complexity c) -> 
                    Map.insert name (ComplexityDefinition c args defn) ft
                Just (TypeAnnotation ts) ->
                    Map.insert name (TypeDefinition ts args defn) ft
                Just (ComplexityType c ts) ->
                    Map.insert name (Complete c ts args defn) ft
                Just _                  ->
                    error $ "Duplicate definition of function " ++ name
                Nothing                 ->
                    Map.insert name (Definition args defn) ft
        buildFunctionTable' ft (Cplx name c)
            = case Map.lookup name ft of
                Just (TypeAnnotation ts) ->
                    Map.insert name (ComplexityType c ts) ft
                Just (Definition args defn) -> 
                    Map.insert name (ComplexityDefinition c args defn) ft
                Just (TypeDefinition ts args defn) ->
                    Map.insert name (Complete c ts args defn) ft
                Just _                  ->
                    error $ "Duplicate complexity annotation for function " ++ name
                Nothing                 ->
                    Map.insert name (Complexity c) ft
        buildFunctionTable' ft (Type name ts)
            = case Map.lookup name ft of
                Just (Complexity c) ->
                    Map.insert name (ComplexityType c ts) ft
                Just (Definition args defn) -> 
                    Map.insert name (TypeDefinition ts args defn) ft
                Just (ComplexityDefinition c args defn) ->
                    Map.insert name (Complete c ts args defn) ft
                Just _                  ->
                    error $ "Duplicate type annotation for function " ++ name
                Nothing                 ->
                    Map.insert name (TypeAnnotation ts) ft


-- buildFunctionTable uses the initial function table to create the final
-- function table, containing the information and which functions to generate,
-- and how they should be generated.
buildFunctionTable :: Int -> InitFunctionTable -> FunctionTable
buildFunctionTable steps
    = Map.foldlWithKey splitFunction Map.empty
    where
        callFunction n = foldl (\app a -> App app (Var a)) (Var n)
        splitFunction :: FunctionTable -> Name -> InitFunctionData -> FunctionTable
        splitFunction ft name (Definition args defn)
            = Map.insert name (Sequential args defn) ft
        splitFunction ft name (TypeDefinition ts args defn)
            = Map.insert name (SequentialT ts args defn) ft
        -- TODO: refactor
        splitFunction ft name (ComplexityDefinition c args defn)
            = case cplxToBoundary c args steps of
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
                        branchingCall 
                            = If cplxExpr (callFunction seqName args) 
                                (callFunction parName args)
                        seqName        = name ++ "_seq"
                        parName        = name ++ "_par"
            where


                sequential    = Sequential args defn
                parallel      = Parallel args (createDependencyGraph args defn)
        splitFunction ft name (Complete c ts args defn)
            = case cplxToBoundaryT c ts args steps of
                Left False     -> 
                    Map.insert name sequential ft
                Left True      ->
                    Map.insert name parallel ft
                Right cplxExpr ->
                    Map.union (Map.fromList [ (name, SequentialT ts args branchingCall)
                                            , (seqName, sequential)
                                            , (parName, parallel)
                                            ]) ft
                    where 
                        branchingCall 
                            = If cplxExpr (callFunction seqName args) 
                                (callFunction parName args)
                        seqName        = name ++ "_seq"
                        parName        = name ++ "_par"
            where
                sequential    = SequentialT ts args defn
                parallel      = ParallelT ts args (createDependencyGraph args defn)
        splitFunction ft _ _ 
            = ft

--- HELPER FUNCTIONS ---

cplxToBoundary :: Cplx -> [Name] -> Int -> Either Bool Expr
cplxToBoundary c args
    = complexityToBoundary (parseComplexity c args)

cplxToBoundaryT :: Cplx -> [Type] -> [Name] -> Int -> Either Bool Expr
cplxToBoundaryT c ts args steps
    = case complexity of
            Constant{} -> boundary
            _          ->
                case cplxType of
                    Bool -> error "Time complexity annotations cannot refer to Bool names"
                    Int  -> boundary
    where
        complexity = parseComplexity c args
        name       = extractName complexity
        cplxType   = head [t | (t, a) <- zip ts args, a == name]
        boundary   = complexityToBoundary complexity steps
        extractName :: Complexity -> Name
        extractName Constant{}           = error "Trying to extract name from constant complexity"
        extractName (Polynomial name _)  = name
        extractName (Exponential _ name) = name
        extractName (Logarithmic name)   = name


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

-- parseComplexity returns the complexity of an annotation if its expressions
-- are supported and variables compatible, else throws an error
parseComplexity :: Cplx -> [Name] -> Complexity
parseComplexity c args
    | fvsValid && sizeValid = parseComplexityExpression c
    | not fvsValid          = error errorFvsMsg
    | otherwise             = error errorSizeMsg
    where
        fvs          = freeVariables c
        fvsValid     = Set.isSubsetOf fvs (Set.fromList args)
        sizeValid    = Set.size fvs <= 1
        errorFvsMsg  = "The time complexity annotation " ++ show c ++ " is incompatible with the function definition"
        errorSizeMsg = "The time complexity annotation " ++ show c ++ " is unsupported as it contains more than 1 name"


-- parseComplexityExpression parses the annotation and returns its associated
-- complexity if it is supported, else throws an error
parseComplexityExpression :: Cplx -> Complexity
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