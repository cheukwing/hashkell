module Middleend.Paralleliser where

import Simple.Syntax
import Frontend (Aggregation(..), Cplx(..), AggregationTable)
import Middleend.DependencyGraph

import Prelude hiding (GT)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Steps = Int
data ParallelisationType
    = Always
    | Branching
    | Never

data EncodingInstruction
    = Sequential (Maybe [Type]) [Name] Expr
    | Parallel (Maybe [Type]) [Name] DependencyGraph

type EncodingInstructionTable = Map Name EncodingInstruction

cplxWorthParallelising :: Steps -> Aggregation -> ParallelisationType
cplxWorthParallelising steps (Just cplx, _, Just _)
    = cplxWorthParallelising' cplx
    where
        cplxWorthParallelising' :: Cplx -> ParallelisationType
        cplxWorthParallelising' (Constant n)
            = if n > steps then Always else Never
        cplxWorthParallelising' Logarithmic{}
            = Never
        cplxWorthParallelising' _
            = Branching
cplxWorthParallelising _ _
    = Never

parallelisationBoundary :: Steps -> Aggregation -> Expr
parallelisationBoundary steps (Just cplx, mts, Just (params, _))
    = case cplx of
        Constant n ->
            if n > steps then Lit (LBool True) else Lit (LBool False)
        Logarithmic{} ->
            Lit (LBool False)
        Polynomial param n ->
            Op GT (lhs param) 
                (Lit (LInt (ceiling $ fromIntegral steps ** (1 / fromIntegral n))))
        Exponential n param ->
            Op GT (lhs param)
                (Lit (LInt (ceiling $ logBase (fromIntegral n) (fromIntegral steps))))
    where
        lhs name = case mts of
                Just ts -> 
                    case head [ t | (t, p) <- zip ts params, p == name] of
                        Int    -> Var name
                        List{} -> App (Var "length") (Var name)
                Nothing -> Var name
parallelisationBoundary _ _
    = Lit (LBool False)


createEncodingInstructionTable :: Steps -> AggregationTable -> EncodingInstructionTable
createEncodingInstructionTable steps
    = foldl (aggregationToInstruction steps) Map.empty . Map.toList

aggregationToInstruction :: Steps -> EncodingInstructionTable -> (Name, Aggregation) -> EncodingInstructionTable
-- no func
aggregationToInstruction _ eit (_, (_, _, Nothing))
    = eit
-- func, but no cplx
aggregationToInstruction _ eit (name, (Nothing, mts, Just (params, defn)))
    = Map.insert name (Sequential mts params defn) eit
-- there is a func and cplx
aggregationToInstruction steps eit (name, agg @ (_, mts, Just (params, defn)))
    = case cplxWorthParallelising steps agg of
        Never ->
            Map.insert name seq eit
        Always ->
            Map.insert name par eit 
        Branching ->
            Map.insert seqName seq
                $ Map.insert parName par
                $ Map.insert name
                    (Sequential mts 
                        params 
                        (If bound 
                            (callFunction parName)
                            (callFunction seqName)))
                    eit
    where
        -- TODO: dg analysis
        dg             = createDependencyGraph params defn
        -- TODO: rename all recursive calls to seqName
        seq            = Sequential mts params defn
        seqName        = name ++ "_seq"
        par            = Parallel mts params dg
        parName        = name ++ "_par"
        bound          = parallelisationBoundary steps agg
        callFunction n = foldl (\app a -> App app (Var a)) (Var n) params