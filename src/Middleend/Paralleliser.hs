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
    | Branching Expr
    | Never
    deriving (Eq, Show)

data EncodingInstruction
    = Sequential (Maybe [Type]) [Name] Expr
    | Parallel (Maybe [Type]) [Name] DependencyGraph
    deriving (Eq, Show)

type EncodingInstructionTable = Map Name EncodingInstruction

parallelisationType :: Steps -> Aggregation -> ParallelisationType
parallelisationType _ (Nothing, _, _)
    = Never
parallelisationType _ (_, _, Nothing)
    = Never
parallelisationType steps (Just (Constant n), _, _)
    = if n > steps then Always else Never
parallelisationType steps (Just Logarithmic{}, _, _)
    = Never
parallelisationType steps (Just cplx, mts, Just (params, _))
    = case cplx of
        Polynomial param n ->
            Branching $ Op GT (lhs param) 
                (Lit (LInt (ceiling $ fromIntegral steps ** (1 / fromIntegral n))))
        Exponential n param ->
            Branching $ Op GT (lhs param)
                (Lit (LInt (ceiling $ logBase (fromIntegral n) (fromIntegral steps))))
    where
        lhs name = case mts of
                Just ts -> 
                    case head [ t | (t, p) <- zip ts params, p == name] of
                        Int    -> Var name
                        List{} -> App (Var "length") (Var name)
                Nothing -> Var name


createEncodingInstructionTable :: Steps -> AggregationTable -> EncodingInstructionTable
createEncodingInstructionTable steps
    = foldl aggToInstr Map.empty . Map.toList
    where 
        aggToInstr :: EncodingInstructionTable -> (Name, Aggregation) -> EncodingInstructionTable
        -- no func
        aggToInstr eit (_, (_, _, Nothing))
            = eit
        -- func, but no cplx
        aggToInstr eit (name, (Nothing, mts, Just (params, defn)))
            = Map.insert name (Sequential mts params defn) eit
        -- there is a func and cplx
        aggToInstr eit (name, agg @ (_, mts, Just (params, defn)))
            = case parallelisationType steps agg of
                Never ->
                    Map.insert name seq eit
                Always ->
                    Map.insert name par eit 
                Branching bound ->
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
                callFunction n = foldl (\app a -> App app (Var a)) (Var n) params