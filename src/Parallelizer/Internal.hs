module Parallelizer.Internal where

import Simple.Syntax
import DependencyGraph

import Prelude hiding (EQ, GT, LT)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Either (Either(..))
import qualified Data.Either as Either
import Control.Monad.Except (throwError, foldM)


type Defn = Expr

-- TODO: support factorial
data Cplx
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
    deriving (Eq, Show)

type InitFunctionTable = Map Name InitFunctionData

type FunctionTable = Map Name FunctionData
data FunctionData
    = Sequential [Name] Defn
    | SequentialT [Type] [Name] Defn
    | Parallel [Name] DependencyGraph
    | ParallelT [Type] [Name] DependencyGraph
    deriving (Eq, Show)

data ParallelizerError
    = IllegalComplexityAnnotation
    | UnsupportedComplexityAnnotation
    | IncompatibleComplexityAnnotation
    | DuplicateDeclaration
    deriving Eq

instance Show ParallelizerError where
    show IllegalComplexityAnnotation = "Illegal expressions in complexity annotation"
    show UnsupportedComplexityAnnotation = "Unsupported expressions in complexity annotation"
    show IncompatibleComplexityAnnotation = "Complexity annotation is incompatible with other function information"
    show DuplicateDeclaration = "Duplicate declaration of function information"

type Parallelizer a = Either ParallelizerError a

createFunctionTable :: Int -> Prog -> FunctionTable
createFunctionTable steps prog
    = case cft of
        Left err -> error (show err)
        Right ft -> ft
    where
        cft = buildInitFunctionTable prog >>= buildFunctionTable steps

-- buildInitFunctionTable builds the initial function table by aggregating
-- each function's parsed declarations.
buildInitFunctionTable :: Prog -> Parallelizer InitFunctionTable
buildInitFunctionTable 
    = foldM buildFunctionTable' Map.empty
    where
        buildFunctionTable' :: InitFunctionTable -> Decl -> Parallelizer InitFunctionTable
        buildFunctionTable' ft (Func name args defn)
            = case Map.lookup name ft of
                Just (Complexity c) -> 
                    return $ Map.insert name (ComplexityDefinition c args defn) ft
                Just (TypeAnnotation ts) ->
                    return $ Map.insert name (TypeDefinition ts args defn) ft
                Just (ComplexityType c ts) ->
                    return $ Map.insert name (Complete c ts args defn) ft
                Just _                  ->
                    throwError DuplicateDeclaration
                Nothing                 ->
                    return $ Map.insert name (Definition args defn) ft
        buildFunctionTable' ft (Cplx name c)
            = do
                cplx <- parseComplexity c
                case Map.lookup name ft of
                    Just (TypeAnnotation ts) ->
                        return $ Map.insert name (ComplexityType cplx ts) ft
                    Just (Definition args defn) -> 
                        return $ Map.insert name (ComplexityDefinition cplx args defn) ft
                    Just (TypeDefinition ts args defn) ->
                        return $ Map.insert name (Complete cplx ts args defn) ft
                    Just _                  ->
                        throwError DuplicateDeclaration
                    Nothing                 ->
                        return $ Map.insert name (Complexity cplx) ft
        buildFunctionTable' ft (Type name ts)
            = case Map.lookup name ft of
                Just (Complexity c) ->
                    return $ Map.insert name (ComplexityType c ts) ft
                Just (Definition args defn) -> 
                    return $ Map.insert name (TypeDefinition ts args defn) ft
                Just (ComplexityDefinition c args defn) ->
                    return $ Map.insert name (Complete c ts args defn) ft
                Just _                  ->
                    throwError DuplicateDeclaration
                Nothing                 ->
                    return $ Map.insert name (TypeAnnotation ts) ft


-- buildFunctionTable uses the initial function table to create the final
-- function table, containing the information and which functions to generate,
-- and how they should be generated.
buildFunctionTable :: Int -> InitFunctionTable -> Parallelizer FunctionTable
buildFunctionTable steps
    = foldM (\a (b, c) -> splitFunction a b c) Map.empty . Map.toList
    where
        callFunction n = foldl (\app a -> App app (Var a)) (Var n)
        splitFunction :: FunctionTable -> Name -> InitFunctionData -> Parallelizer FunctionTable
        splitFunction ft name (Definition args defn)
            = return $ Map.insert name (Sequential args defn) ft
        splitFunction ft name (TypeDefinition ts args defn)
            = return $ Map.insert name (SequentialT ts args defn) ft
        splitFunction ft name (ComplexityDefinition cplx args defn) = do
            validateComplexityNames cplx args
            boundary <- complexityToBoundary cplx Int steps
            let
                s @ (seqName, sequential) 
                    = (name ++ "_seq", Sequential args defn)
                p @ (parName, parallel)
                     = (name ++ "_par", Parallel args (createDependencyGraph args defn))
                branch = (name, Sequential args (If boundary (callFunction parName args) (callFunction seqName args)))
            case (isTrivialBoundary boundary, fromTrivialShouldParallelise boundary) of
                (True, True)  -> return $ Map.insert name parallel ft
                (True, False) -> return $ Map.insert name sequential ft
                _             -> return $ Map.union (Map.fromList [ branch, s, p ]) ft
        splitFunction ft name (Complete cplx ts args defn) = do
            validateComplexityNames cplx args
            let t = case cplx of
                        Constant{} -> Int
                        _          -> getNameType ts args cplx
            boundary <- complexityToBoundary cplx t steps
            let
                s @ (seqName, sequential) 
                    = (name ++ "_seq", SequentialT ts args defn)
                p @ (parName, parallel)
                    = (name ++ "_par", ParallelT ts args (createDependencyGraph args defn))
                branch = (name, SequentialT ts args (If boundary (callFunction parName args) (callFunction seqName args)))
            case (isTrivialBoundary boundary, fromTrivialShouldParallelise boundary) of
                (True, True)  -> return $ Map.insert name parallel ft
                (True, False) -> return $ Map.insert name sequential ft
                _             -> return $ Map.union (Map.fromList [ branch, s, p ]) ft
        splitFunction ft _ _ 
            = return ft

--- HELPER FUNCTIONS ---

-- getNameType gets the type of the name in the complexity
getNameType :: [Type] -> [Name] -> Cplx -> Type
getNameType ts args cplx
    = head [t | (t, a) <- zip ts args, a == name]
    where
        name = extractComplexityName cplx


-- fromTrivialShouldParallelise returns true if the trivial boundary dictates
-- that the parallelised version will always be run
fromTrivialShouldParallelise :: Expr -> Bool
fromTrivialShouldParallelise (Lit (LBool t)) 
    = t
fromTrivialShouldParallelise _
    = error "Not a trivial boundary"


-- isTrivialBoundary returns true if the boundary is just a bool
isTrivialBoundary :: Expr -> Bool
isTrivialBoundary (Lit LBool{}) = True
isTrivialBoundary _             = False


-- validateComplexityNames checks that the complexity's name is part of the
-- function's arguments
validateComplexityNames :: Cplx -> [Name] -> Parallelizer ()
validateComplexityNames Constant{} _ 
    = return ()
validateComplexityNames c args =
    if extractComplexityName c `elem` args
        then return ()
        else throwError IncompatibleComplexityAnnotation


-- extractComplexityName gets the name from the complexity,
-- undefined for Constants
extractComplexityName :: Cplx -> Name
extractComplexityName (Polynomial name _)  = name
extractComplexityName (Exponential _ name) = name
extractComplexityName (Logarithmic name)   = name


-- complexityToBoundary takes a complexity, the type of the name, and the step
-- approximation to determine the boundary. 
complexityToBoundary :: Cplx -> Type -> Int -> Parallelizer Expr
complexityToBoundary (Constant n) _ steps
    = return $ Lit (LBool (n > steps))
complexityToBoundary _ Bool _ 
    -- Cannot create a boundary if the name refers to a bool
    = throwError IncompatibleComplexityAnnotation
complexityToBoundary c Int steps
    = return $ case c of
        Polynomial name n ->
            Op GT (Var name) (Lit (LInt (ceiling $ fromIntegral steps ** (1 / fromIntegral n))))
        Exponential n name ->
            Op GT (Var name) (Lit (LInt (ceiling $ logBase (fromIntegral n) (fromIntegral steps))))
        Logarithmic name ->
            --Op LT (Var name) (Lit (LInt (2 ^ steps)))
            -- Unlikely for steps to ever be small enough that n > 2^steps
            Lit (LBool False)


-- parseComplexityExpression parses the annotation and returns its associated
-- complexity if it is supported, else throws an error
parseComplexity :: Expr -> Parallelizer Cplx
parseComplexity If{}
    = throwError IllegalComplexityAnnotation
parseComplexity Let{} 
    = throwError IllegalComplexityAnnotation
parseComplexity (App (Var "log") (Var name)) 
    = return (Logarithmic name)
parseComplexity (App (Var "log") _) 
    = throwError UnsupportedComplexityAnnotation
parseComplexity App{}
    = throwError IllegalComplexityAnnotation
parseComplexity (Var name)
    = return (Polynomial name 1)
parseComplexity (Lit (LInt n))
    = return (Constant n)
parseComplexity Lit{}
    = throwError IllegalComplexityAnnotation
parseComplexity (Op Exp (Var name) (Lit (LInt n)))
    = return (Polynomial name n)
parseComplexity (Op Exp (Lit (LInt n)) (Var name))
    = return (Exponential n name)
parseComplexity Op{}
    = throwError UnsupportedComplexityAnnotation


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