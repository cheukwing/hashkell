module Frontend.Aggregator where

import Simple.Syntax
import Frontend.Complexity
import Frontend.Error

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.Except (foldM, throwError)

type Defn = Expr
type Aggregation = (Maybe Cplx, Maybe [Type], Maybe ([Name], Defn))
type AggregationTable = Map Name Aggregation

aggregate :: Prog -> Either Error AggregationTable
aggregate = foldM aggregate' Map.empty

aggregate' :: AggregationTable ->  Decl -> Either Error AggregationTable 
aggregate' at (Func name params defn)
    = case Map.lookup name at of
        Just (mcplx, mts, Nothing) ->
            return $ Map.insert name (mcplx, mts, Just (params, defn)) at
        Just (_, _, Just _) ->
            throwError DuplicateDeclaration
        Nothing ->
            return $ Map.insert name (Nothing, Nothing, Just (params, defn)) at
aggregate' at (Cplx name c) = do
    cplx <- parseComplexity c
    case Map.lookup name at of
        Just (Nothing, mts, mfunc) ->
            return $ Map.insert name (Just cplx, mts, mfunc) at
        Just (Just _, _, _) ->
            throwError DuplicateDeclaration
        Nothing ->
            return $ Map.insert name (Just cplx, Nothing, Nothing) at
aggregate' at (Type name ts)
    = case Map.lookup name at of
        Just (mcplx, Nothing, mfunc) ->
            return $ Map.insert name (mcplx, Just ts, mfunc) at
        Just (_, Just _, _) ->
            throwError DuplicateDeclaration
        Nothing ->
            return $ Map.insert name (Nothing, Just ts, Nothing) at