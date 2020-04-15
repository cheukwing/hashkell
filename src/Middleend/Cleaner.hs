module Middleend.Cleaner where

import Simple.Syntax
import Frontend (AggregationTable, Aggregation(..))

import Control.Monad.State.Strict
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (isPrefixOf, partition)
import Data.Maybe (Maybe)
import qualified Data.Maybe as Maybe

cleanup :: AggregationTable -> AggregationTable
cleanup = Map.map cleanup'
    where
        cleanup' :: Aggregation -> Aggregation
        cleanup' (mcplx, mts, Just (params, e))
            = (mcplx, mts, Just (finalParams, e''))
            where
                (finalParams, e') = ensureUniqueNames params e
                e''               = ensureNoUnusedDefs e''
        cleanup' agg
            = agg


type Counter = Int
type UniqueState = (Set Name, Map Name Name, Counter)

ensureUniqueNames :: [Name] -> Expr -> ([Name], Expr)
ensureUniqueNames params e 
    = (finalParams, evalState (uniqueNamer e) initState)
    where
        similarityCheck = zip params (map isSimilarToGeneratedNames params)
        paramCountUsed  = scanl (\acc (_, s) -> if s then acc + 1 else acc)
                            0
                            similarityCheck
        mappings        = map (\((p, s), c) -> 
                            if s
                                then (p, "_y" ++ show c)
                                else (p, p))
                            (zip similarityCheck paramCountUsed)
        finalParams     = map snd mappings
        initState       = ( Set.fromList finalParams
                          , Map.fromList (filter (uncurry (/=)) mappings)
                          , last paramCountUsed
                          )



isSimilarToGeneratedNames :: String -> Bool
isSimilarToGeneratedNames name
    = ("_x" `isPrefixOf` name) || ("_y" `isPrefixOf` name) 

updateUniqueState :: Def -> State UniqueState ()
updateUniqueState (Def n _) = do
    (s, m, c) <- get
    if Set.member n s || isSimilarToGeneratedNames n
        then put (s, Map.insert n ("_y" ++ show c) m, c + 1)
        else put (Set.insert n s, m, c)

toUniqueName :: Name -> State UniqueState Name
toUniqueName n = do
    (_, m, _) <- get
    return (Map.findWithDefault n n m)


uniqueNamer :: Expr -> State UniqueState Expr
uniqueNamer e @ Lit{} = return e
uniqueNamer (Var n) = Var <$> toUniqueName n
uniqueNamer (Op op e1 e2) = do
    e1' <- uniqueNamer e1
    e2' <- uniqueNamer e2
    return (Op op e1' e2')
uniqueNamer (App e1 e2) = do
    e1' <- uniqueNamer e1
    e2' <- uniqueNamer e2
    return (App e1' e2')
uniqueNamer (If e1 e2 e3) = do
    e1' <- uniqueNamer e1
    e2' <- uniqueNamer e2
    e3' <- uniqueNamer e3
    return (If e1' e2' e3')
uniqueNamer (Let defs e) = do
    (_, m, _) <- get
    mapM_ updateUniqueState defs
    -- setup the names for duplicated names in this scope
    defs' <- mapM (\(Def n e) -> Def <$> toUniqueName n <*> uniqueNamer e) defs
    e' <- uniqueNamer e
    (s, _, c) <- get
    -- we want uniqueness for the entire expression, but the renamings are only
    -- relevant for the scope in `e`; we restore the old renamings
    put (s, m, c)
    return (Let defs' e')

-- pre: all names are unique (use ensureUniqueNames)
ensureNoUnusedDefs :: Expr -> Expr
ensureNoUnusedDefs e = removeUnusedDefs (usedDefs e) e

usedDefs :: Expr -> Set Name
usedDefs Lit{} 
    = Set.empty
usedDefs (Var n) 
    = Set.singleton n
usedDefs (Op _ e1 e2) 
    = Set.union (usedDefs e1) (usedDefs e2)
usedDefs (App e1 e2)
    = Set.union (usedDefs e1) (usedDefs e2)
usedDefs (Let defs e)
    = Set.unions (usedDefs e : map (\(Def _ e) -> usedDefs e) defs)
usedDefs (If e1 e2 e3)
    = Set.unions [usedDefs e1, usedDefs e2, usedDefs e3]

removeUnusedDefs :: Set Name -> Expr -> Expr
removeUnusedDefs _ e @ Lit{}
    = e
removeUnusedDefs _ e @ Var{}
    = e
removeUnusedDefs used (Op op e1 e2)
    = Op op (removeUnusedDefs used e1) (removeUnusedDefs used e2)
removeUnusedDefs used (App e1 e2)
    = App (removeUnusedDefs used e1) (removeUnusedDefs used e2)
removeUnusedDefs used (Let defs e)
    | null defs' = e'
    | otherwise  = Let defs' e'
    where 
        e'    = removeUnusedDefs used e
        defs' = map (\(Def n e) -> Def n (removeUnusedDefs used e)) 
                $ filter (\(Def n _) -> Set.member n used) defs
removeUnusedDefs used (If e1 e2 e3)
    = If (removeUnusedDefs used e1)
        (removeUnusedDefs used e2)
        (removeUnusedDefs used e3)