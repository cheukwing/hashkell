module Middleend.Paralleliser where

import Hashkell.Syntax
import Frontend (FunctionData(..), Cplx(..), FunctionTable)
import Middleend.DependencyGraph
import Context

import Prelude hiding (GT)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (partition)

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
parallelisationType :: Steps -> FunctionData -> ParallelisationType
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
            -- param! > steps => param >= argmin {x | x! > steps}
            Branching $ Op GTE (lhs param)
                (Lit (LInt (snd . head $ dropWhile (flip (<) steps . fst) $ scanl (\(f, i) n -> (f * n, i + 1)) (1, 0) [1..])))
    where
        lhs name = case mts of
                Just ts -> 
                    case head [ t | (t, p) <- zip ts params, p == name] of
                        Int    -> Var name
                        List{} -> App (Var "length") (Var name)
                Nothing -> Var name


-- parAndTriTables returns a table with the parallelisation
-- type of each function, and the triviality of each function (based on the
-- parallelisationType)
parAndTriTables :: Steps -> FunctionTable -> (Map Name ParallelisationType, Map Name Bool)
parAndTriTables steps ft
    = (parTable, triTable)
    where
        parTable = Map.map (parallelisationType steps) ft
        triTable = Map.map (== Never) parTable


-- createEncodingInstructionTable takes each aggregation and creates encoding
-- instructions, which tells the code generator to generate a certain function,
-- and how to generate that function into Haskell, i.e. whether in sequential
-- or parallel
createEncodingInstructionTable :: Context -> FunctionTable -> EncodingInstructionTable
createEncodingInstructionTable ctx ft
    = foldl aggToInstr Map.empty . Map.toList $ ft
    where
        (parTable, triTable) = parAndTriTables (ctxSteps ctx) ft
        aggToInstr :: EncodingInstructionTable -> (Name, FunctionData) -> EncodingInstructionTable
        -- If there is no definition, do not bother encoding ft all
        aggToInstr eit (_, (_, _, Nothing))
            = eit
        -- If we have no complexity, just encode it as it was (sequentially)
        aggToInstr eit (name, (Nothing, mts, Just (params, defn)))
            = Map.insert name (Sequential mts params defn) eit
        -- If we have a complexity and definition, then assess whether it is
        -- worth parallelising, then add the relevant encoding instructions
        aggToInstr eit (name, agg @ (_, mts, Just (params, defn)))
            = case ((Map.!) parTable name, hasParallelism dg) of
                -- If should always parallelise, and parallelism exists
                (Always, True) ->
                    Map.insert name par eit 
                -- If should branching parallelise, and parallelism exists
                (Branching bound, True) ->
                    Map.insert seqName seq
                        $ Map.insert parName par
                        $ Map.insert name branchingCall eit
                    where
                        seq = Sequential mts params
                            (replaceRecursiveCalls name defn)
                        branchingCall = Sequential mts params
                            (If bound (callFunction parName)
                                (callFunction seqName))
                -- If should never parallelise, or no parallelism exists
                (_, _) ->
                    Map.insert name (Sequential mts params defn) eit
            where
                dg             = createDependencyGraph ctx triTable params defn
                seqName        = name ++ "_seq"
                par            = Parallel mts params dg
                parName        = name ++ "_par"
                callFunction n = foldl (\app a -> App app (Var a)) (Var n) params

createEncodingInstructionTableAll :: Context -> FunctionTable -> EncodingInstructionTable
createEncodingInstructionTableAll ctx ft
    = foldl aggToInstr Map.empty . Map.toList $ ft
    where
        (_, triTable) = parAndTriTables (ctxSteps ctx) ft
        aggToInstr :: EncodingInstructionTable -> (Name, FunctionData) -> EncodingInstructionTable
        aggToInstr eit (_, (_, _, Nothing))
            = eit
        aggToInstr eit (name, (_, mts, Just (params, defn)))
            = Map.insert name (Parallel mts params (createDependencyGraph ctx triTable params defn)) eit


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


-- hasParallelism checks if there are any parallel computations from the
-- DependencyGraph built from a function definition, i.e. any node with more
-- than one Dep arc
-- NOTE: the use of let definitions, e.g.
--           Scope
--          /     \
--      a = 1     b = 1
--          \    /
--          a + b
-- will appear as parallelism, even though their calculations are atomic
-- due to the limitations of the dependency graph
hasParallelism :: DependencyGraph -> Bool
hasParallelism (ns, ds)
    = hasParallelism' "_"
    where
        hasParallelism' :: Name -> Bool
        hasParallelism' n
            = case ns Map.! n of
                Conditional{}
                    -- branching from non-scope children
                    | length exps > 1  -> True
                    -- if no non-scope children, then just check scopes
                    | null exps        -> scopesArePar
                    -- if one non-scope child, check scopes and that child
                    | length exps == 1 -> scopesArePar || expIsPar
                    where
                        expIsPar       = hasParallelism' (head exps)
                        scopesArePar   = any hasParallelism' scopes
                        (scopes, exps) = (\(a, b) -> (map fst a, map fst b)) $
                            partition (\(_, t) -> t == DepThen || t == DepElse)
                                allChildren
                        allChildren    = Set.toList 
                            $ Set.map (\(_, c, t) -> (c, t))
                            $ Set.filter (\(p, _, _) -> p == n) ds

                Expression (DHighApp _ DApp{} _) -> True

                _  -- otherwise if not Conditional node
                    | null children       -> False
                    | length children > 1 -> True
                    | otherwise           -> hasParallelism' (head children)
                    where
                        children = Set.toList 
                            $ Set.map (\(_, c, _) -> c)
                            $ Set.filter (\(p, _, _) -> p == n) ds
