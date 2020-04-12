module Frontend.Aggregator where

import Simple.Syntax
import Frontend.Complexity
import Frontend.Error

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.Except (foldM, throwError)

type Defn = Expr

data Aggregation
    = Complexity Cplx
    | TypeAnnotation [Type]
    | Definition [Name] Defn
    | ComplexityType Cplx [Type]
    | ComplexityDefinition Cplx [Name] Defn
    | TypeDefinition [Type] [Name] Defn
    | Complete Cplx [Type] [Name] Defn
    deriving (Eq, Show)

type AggregationTable = Map Name Aggregation

aggregate :: Prog -> Either Error AggregationTable
aggregate = foldM aggregate' Map.empty

aggregate' :: AggregationTable ->  Decl -> Either Error AggregationTable 
aggregate' at (Func name params defn)
    = case Map.lookup name at of
        Just (Complexity c) -> 
            return $ Map.insert name (ComplexityDefinition c params defn) at
        Just (TypeAnnotation ts) ->
            return $ Map.insert name (TypeDefinition ts params defn) at
        Just (ComplexityType c ts) ->
            return $ Map.insert name (Complete c ts params defn) at
        Just _                  ->
            throwError DuplicateDeclaration
        Nothing                 ->
            return $ Map.insert name (Definition params defn) at
aggregate' at (Cplx name c)
    = do
        cplx <- parseComplexity c
        case Map.lookup name at of
            Just (TypeAnnotation ts) ->
                return $ Map.insert name (ComplexityType cplx ts) at
            Just (Definition params defn) -> 
                return $ Map.insert name (ComplexityDefinition cplx params defn) at
            Just (TypeDefinition ts params defn) ->
                return $ Map.insert name (Complete cplx ts params defn) at
            Just _                  ->
                throwError DuplicateDeclaration
            Nothing                 ->
                return $ Map.insert name (Complexity cplx) at
aggregate' at (Type name ts)
    = case Map.lookup name at of
        Just (Complexity c) ->
            return $ Map.insert name (ComplexityType c ts) at
        Just (Definition params defn) -> 
            return $ Map.insert name (TypeDefinition ts params defn) at
        Just (ComplexityDefinition c params defn) ->
            return $ Map.insert name (Complete c ts params defn) at
        Just _                  ->
            throwError DuplicateDeclaration
        Nothing                 ->
            return $ Map.insert name (TypeAnnotation ts) at