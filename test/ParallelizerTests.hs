module ParallelizerTests (parallelizerTests) where

import Parallelizer.Internal (freeVariables, parseComplexityExpression, Complexity(..))
import Simple.Syntax

import qualified Data.Set as Set

import Test.Tasty
import Test.Tasty.HUnit

parallelizerTests :: TestTree
parallelizerTests = testGroup "Parallelizer Tests" 
            [ freeVariablesTests
            , parseComplexityExpressionTests
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

parseComplexityExpressionTests = testGroup "parseComplexityExpression tests"
    [ testCase "parses logarithmic time" $
        parseComplexityExpression (App (Var "log") (Var "n"))
            @?= Logarithmic "n"
    , testCase "parses linear time" $
        parseComplexityExpression (Var "n")
            @?= Polynomial "n" 1
    , testCase "parses quadratic time" $
        parseComplexityExpression (Op Exp (Var "n") (Lit (LInt 2)))
            @?= Polynomial "n" 2
    , testCase "parses exponential time" $
        parseComplexityExpression (Op Exp (Lit (LInt 2)) (Var "n") )
            @?= Exponential 2 "n"
    , testCase "parses constant time" $
        parseComplexityExpression (Lit (LInt 100))
            @?= Constant 100
    ]