module FrontendTests (frontendTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Prelude hiding (EQ, LT, GT)
import Data.Either (Either(..))

import Simple.Syntax
import Frontend.Complexity (parseComplexity, Cplx(..))
import Frontend.Verification (verifyAggregation)
import Frontend.Error (Error(..))

frontendTests :: TestTree
frontendTests = testGroup "Frontend Tests"
    [ parseComplexityTests
    , verifyAggregationTests
    ]

parseComplexityTests = testGroup "parseComplexity tests"
    [ testCase "logarithmic time is parsed" $
        parseComplexity (App (Var "log") (Var "n"))
            @?= Right (Logarithmic "n")
    , testCase "linear time is parsed" $
        parseComplexity (Var "n")
            @?= Right (Polynomial "n" 1)
    , testCase "quadratic time is parsed" $
        parseComplexity (Op Exp (Var "n") (Lit (LInt 2)))
            @?= Right (Polynomial "n" 2)
    , testCase "exponential time is parsed" $
        parseComplexity (Op Exp (Lit (LInt 2)) (Var "n") )
            @?= Right (Exponential 2 "n")
    , testCase "constant time is parsed" $
        parseComplexity (Lit (LInt 100))
            @?= Right (Constant 100)
    , testCase "complexity with if is illegal" $
        parseComplexity (If (Op GT (Var "n") (Lit (LInt 10))) (Var "n") (Lit (LInt 100)))
            @?= Left IllegalComplexity
    , testCase "complexity with let is illegal" $
        parseComplexity (Let [Def "m" (Lit (LInt 2))] (Op Exp (Var "n") (Var "m")))
            @?= Left IllegalComplexity
    , testCase "log applied to non-var is unsupported" $
        parseComplexity (App (Var "log") (Lit (LInt 16)))
            @?= Left UnsupportedComplexity
    , testCase "arbitrary applications are illegal" $
        parseComplexity (App (Var "frog") (Var "n"))
            @?= Left IllegalComplexity
    , testCase "arbitrary literals are illegal" $
        parseComplexity (Lit (LBool True))
            @?= Left IllegalComplexity
    , testCase "arbitrary operations are unsupported" $
        parseComplexity (Op Exp (Var "n") (Var "m"))
            @?= Left UnsupportedComplexity
    ]

verifyAggregationTests = testGroup "verifyAggregation tests"
    [ testCase "complexity with param not present in function params is incompatible" $
        verifyAggregation (Just (Polynomial "n" 2), Nothing, Just (["m"], Lit (LInt 1)))
            @?= Left IncompatibleComplexity
    , testCase "complexity with param not present in function params is incompatible (typed)" $
        verifyAggregation (Just (Polynomial "n" 2), Just [Int, Int], Just (["m"], Lit (LInt 1)))
            @?= Left IncompatibleComplexity
    , testCase "complexity with boolean param is incompatible" $
        verifyAggregation (Just (Polynomial "n" 2), Just [Bool, Int], Just (["n"], Lit (LInt 1)))
            @?= Left IncompatibleComplexity
    , testCase "complexity with int param is compatible" $
        verifyAggregation (Just (Polynomial "n" 2), Just [Int, Int], Just (["n"], Lit (LInt 1)))
            @?= Right ()
    , testCase "complexity with list param is compatible" $
        verifyAggregation (Just (Polynomial "n" 2), Just [List Bool, Int], Just (["n"], Lit (LInt 1)))
            @?= Right ()
    , testCase "complexity with param present in function params is incompatible (untyped)" $
        verifyAggregation (Just (Polynomial "n" 2), Nothing, Just (["n"], Lit (LInt 1)))
            @?= Right ()
    ]