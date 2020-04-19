module Middleend.Paralleliser where

import Simple.Syntax
import Frontend (Aggregation(..), Cplx(..), AggregationTable)
import Middleend.DependencyGraph

import Prelude hiding (GT)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Steps = Int
data ParallelisationType
    = Always -- Always parallelise
    | Branching Expr -- Parallelise if the condition is met
    | Never -- Never parallelise
    deriving (Eq, Show)

data EncodingInstruction
    = Sequential (Maybe [Type]) [Name] Expr
    | Parallel (Maybe [Type]) [Name] DependencyGraph
    deriving (Eq, Show)

type EncodingInstructionTable = Map Name EncodingInstruction

-- parallelisationType takes the steps approximating the overhead from
-- parallelisation, and the aggregation of a function, to determine whether
-- it is worth parallelising the function
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
            -- param^n > steps => param >= ceil (n_root steps)
            Branching $ Op GTE (lhs param) 
                (Lit (LInt (ceiling $ fromIntegral steps ** (1 / fromIntegral n))))
        Exponential n param ->
            -- n^param > steps => param >= ceil (log_n steps)
            Branching $ Op GTE (lhs param)
                (Lit (LInt (ceiling $ logBase (fromIntegral n) (fromIntegral steps))))
        Factorial param ->
            -- param! > steps => param >= n (where n! > steps)
            Branching $ Op GTE (lhs param)
                (Lit (LInt (snd . head $ dropWhile (flip (<) 1000 . fst) $ scanl (\(f, i) n -> (f * n, i + 1)) (1, 0) [1..])))
    where
        lhs name = case mts of
                Just ts -> 
                    case head [ t | (t, p) <- zip ts params, p == name] of
                        Int    -> Var name
                        List{} -> App (Var "length") (Var name)
                Nothing -> Var name


-- createEncodingInstructionTable takes each aggregation and creates encoding
-- instructions, which tells the code generator to generate a certain function,
-- and how to generate that function into Haskell, i.e. whether in sequential
-- or parallel
createEncodingInstructionTable :: Steps -> AggregationTable -> EncodingInstructionTable
createEncodingInstructionTable steps
    = foldl aggToInstr Map.empty . Map.toList
    where 
        aggToInstr :: EncodingInstructionTable -> (Name, Aggregation) -> EncodingInstructionTable
        -- If there is no definition, do not bother encoding at all
        aggToInstr eit (_, (_, _, Nothing))
            = eit
        -- If we have no complexity, just encode it as it was (sequentially)
        aggToInstr eit (name, (Nothing, mts, Just (params, defn)))
            = Map.insert name (Sequential mts params defn) eit
        -- If we have a complexity and definition, then assess whether it is
        -- worth parallelising, then add the relevant encoding instructions
        aggToInstr eit (name, agg @ (_, mts, Just (params, defn)))
            = case parallelisationType steps agg of
                Never -> -- Never parallelise, then encode sequentially
                    Map.insert name (Sequential mts params defn) eit
                Always -> -- Always parallelise, then encode parallel
                    Map.insert name par eit 
                Branching bound -> -- Parallelise if the condition is met
                    Map.insert seqName seq
                        $ Map.insert parName par
                        $ Map.insert name branchingCall eit
                    where
                        seq = Sequential mts params
                            (replaceRecursiveCalls name defn)
                        branchingCall = Sequential mts params
                            (If bound (callFunction parName)
                                (callFunction seqName))
            where
                -- TODO: dg analysis
                dg             = createDependencyGraph params defn
                seqName        = name ++ "_seq"
                par            = Parallel mts params dg
                parName        = name ++ "_par"
                callFunction n = foldl (\app a -> App app (Var a)) (Var n) params


-- replaceRecursiveCalls changes all the calls to the given name to
-- "{name}_seq", to prevent calls back to the branch
-- recursive calls will most likely use smaller arguments, so if the sequential
-- branch is met, then it is likely that future calls will also meet the
-- sequential branch
-- thus, we avoid the check by calling the sequential branch directly
replaceRecursiveCalls :: Name -> Expr -> Expr
replaceRecursiveCalls _ e @ Lit{}
    = e
replaceRecursiveCalls n e @ (Var v)
    | n == v    = Var (n ++ "_seq")
    | otherwise = e
replaceRecursiveCalls n (App e1 e2)
    = App (replaceRecursiveCalls n e1) (replaceRecursiveCalls n e2)
replaceRecursiveCalls n (Op op e1 e2)
    = Op op (replaceRecursiveCalls n e1) (replaceRecursiveCalls n e2)
replaceRecursiveCalls n (Let ds e)
    = Let (map (\(Def d de) -> Def d (replaceRecursiveCalls n de)) ds)
        (replaceRecursiveCalls n e)
replaceRecursiveCalls n (If e1 e2 e3)
    = If (replaceRecursiveCalls n e1) 
        (replaceRecursiveCalls n e2)
        (replaceRecursiveCalls n e3)
