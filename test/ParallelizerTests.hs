module ParallelizerTests (parallelizerTests) where

import Parallelizer.Internal (freeVariables)
import Simple.Syntax

import qualified Data.Set as Set

import Test.Tasty
import Test.Tasty.HUnit

parallelizerTests :: TestTree
parallelizerTests = testGroup "Parallelizer Tests" 
            [ freeVariablesTests
            ]


freeVariablesTests = testGroup "freeVariables tests"
    [ testCase "finds free variables from operation" $
        freeVariables (Op Add (Op Mul (Var "b") (Var "c")) (Var "d"))
            @?= Set.fromList ["b", "c", "d"]
    , testCase "finds free variables from application" $
        freeVariables (App (App (Var "bungletron") (Var "bungletronics")) (Lit (LInt 10)))
            @?= Set.fromList ["bungletron", "bungletronics"]
    , testCase "bound variables are not included" $
        freeVariables (Let [Def "a" (Lit (LInt 1)), Def "b" (Lit (LInt 2))] (Op Mul (Op Mul (Var "a") (Var "b")) (Var "c")))
            @?= Set.singleton "c"
    ]