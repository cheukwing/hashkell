module Middleend.Cleaner where


import Frontend (AggregationTable)

import Control.Monad.State.Strict
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (isPrefixOf)
import Data.Maybe (Maybe)
import qualified Data.Maybe as Maybe
import Simple.Syntax


type Counter = Int

type UniqueState = (Set Name, Map Name Name, Counter)

ensureUniqueNames :: Expr -> Expr
ensureUniqueNames e 
    = evalState (uniqueNamer e) (Set.empty, Map.empty, 0)

updateUniqueState :: Def -> State UniqueState ()
updateUniqueState (Def n _) = do
    (s, m, c) <- get
    if Set.member n s || "_x" `isPrefixOf` n || "_y" `isPrefixOf` n
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
    (s, m, _) <- get
    mapM_ updateUniqueState defs
    defs' <- mapM (\(Def n e) -> Def <$> toUniqueName n <*> uniqueNamer e) defs
    e' <- uniqueNamer e
    (_, _, c) <- get
    put (s, m, c)
    return (Let defs' e')