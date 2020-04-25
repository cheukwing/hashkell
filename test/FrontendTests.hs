module FrontendTests (frontendTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Prelude hiding (EQ, LT, GT)
import Data.Either (Either(..))
import qualified Data.Map.Strict as Map

import Hashkell.Syntax
import Frontend.Complexity (parseComplexity, Cplx(..))
import Frontend.Verification (verifyFunctionData, toFunctionData)
import Frontend.Aggregator (aggregate)
import Frontend.Error (Error(..))

frontendTests :: TestTree
frontendTests = testGroup "Frontend Tests"
    [ parseComplexityTests
    , verifyFunctionDataTests
    , toFunctionDataTests
    , aggregateTests
    ]

parseComplexityTests = testGroup "parseComplexity tests"
    [ testCase "logarithmic time is parsed" $
        parseComplexity (App (Var "log") (Var "n"))
            @?= Right (Logarithmic "n")
    , testCase "factorial time is parsed" $
        parseComplexity (App (Var "fac") (Var "n"))
            @?= Right (Factorial "n")
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
    , testCase "fac applied to non-var is unsupported" $
        parseComplexity (App (Var "fac") (Lit (LInt 16)))
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

verifyFunctionDataTests = testGroup "verifyFunctionData tests"
    [ testCase "complexity with param not present in function params is incompatible" $
        verifyFunctionData (Just (Polynomial "n" 2), Nothing, Just (["m"], Lit (LInt 1)))
            @?= Left IncompatibleComplexity
    , testCase "complexity with param not present in function params is incompatible (typed)" $
        verifyFunctionData (Just (Polynomial "n" 2), Just [Int, Int], Just (["m"], Lit (LInt 1)))
            @?= Left IncompatibleComplexity
    , testCase "complexity with boolean param is incompatible" $
        verifyFunctionData (Just (Polynomial "n" 2), Just [Bool, Int], Just (["n"], Lit (LInt 1)))
            @?= Left IncompatibleComplexity
    , testCase "complexity with int param is compatible" $
        verifyFunctionData (Just (Polynomial "n" 2), Just [Int, Int], Just (["n"], Lit (LInt 1)))
            @?= Right (Just (Polynomial "n" 2), Just [Int, Int], Just (["n"], Lit (LInt 1)))
    , testCase "complexity with list param is compatible" $
        verifyFunctionData (Just (Polynomial "n" 2), Just [List Bool, Int], Just (["n"], Lit (LInt 1)))
            @?= Right (Just (Polynomial "n" 2), Just [List Bool, Int], Just (["n"], Lit (LInt 1)))
    , testCase "complexity with param present in function params is incompatible (untyped)" $
        verifyFunctionData (Just (Polynomial "n" 2), Nothing, Just (["n"], Lit (LInt 1)))
            @?= Right (Just (Polynomial "n" 2), Nothing, Just (["n"], Lit (LInt 1)))
    ]

toFunctionDataTests = testGroup "toFunctionData tests"
    [ testCase "fails when complexity uses illegal expressions" $
        toFunctionData (Just $ If (Lit (LBool True)) (Var "n") (Op Exp (Var "n") (Lit (LInt 2))), Nothing, Just (["n"], basicFuncDefn))
        @?= Left IllegalComplexity
    , testCase "fails when complexity uses unsupported expressions" $
        toFunctionData (Just $ Op Add (Var "n") (Var "m"), Nothing, Just (["n"], basicFuncDefn))
        @?= Left UnsupportedComplexity
    , testCase "converts simple expression to complexity" $
        toFunctionData (Just $ Var "n", Nothing, Just (["n"], basicFuncDefn))
        @?= Right (Just (Polynomial "n" 1), Nothing, Just (["n"], basicFuncDefn))
    , testCase "converts complex expression to complexity" $
        toFunctionData (Just $ Op Exp (Lit (LInt 2)) (Var "n"), Nothing, Just (["n"], basicFuncDefn))
        @?= Right (Just (Exponential 2 "n"), Nothing, Just (["n"], basicFuncDefn))
    ]

basicFuncDefn = Op Add (Lit (LInt 1)) (Lit (LInt 1))
basicFuncDecl = Func "foo" ["n"] basicFuncDefn
basicCplxDecl = Cplx "foo" (Var "n")
basicTypeDecl = Type "foo" [Int, Int]

aggregateTests = testGroup "aggregate tests"
    [ testCase "forms complete when given cplx, type, func" $
        aggregate [basicCplxDecl, basicTypeDecl, basicFuncDecl]
            @?= Right (Map.fromList [("foo", (Just (Var "n"), Just [Int, Int], Just (["n"], basicFuncDefn)))])
    , testCase "forms complete when given type, cplx, func" $
        aggregate [basicTypeDecl, basicCplxDecl, basicFuncDecl]
            @?= Right (Map.fromList [("foo", (Just (Var "n"), Just [Int, Int], Just (["n"], basicFuncDefn)))])
    , testCase "forms complete when given func, cplx, type" $
        aggregate [basicFuncDecl, basicCplxDecl, basicTypeDecl]
            @?= Right (Map.fromList [("foo", (Just (Var "n"), Just [Int, Int], Just (["n"], basicFuncDefn)))])
    , testCase "forms partial when given func, type" $
        aggregate [basicFuncDecl, basicTypeDecl]
            @?= Right (Map.fromList [("foo", (Nothing, Just [Int, Int], Just (["n"], basicFuncDefn)))])
    , testCase "forms partial when given cplx, type" $
        aggregate [basicCplxDecl, basicTypeDecl]
            @?= Right (Map.fromList [("foo", (Just (Var "n"), Just [Int, Int], Nothing))])
    , testCase "forms only func when given func" $
        aggregate [basicFuncDecl]
            @?= Right (Map.fromList [("foo", (Nothing, Nothing, Just (["n"], basicFuncDefn)))])
    , testCase "fails when duplicate declarations present" $
        aggregate [basicFuncDecl, basicTypeDecl, Type "foo" [Int, Bool]]
            @?= Left DuplicateDeclaration
    , testCase "forms multiple entries for different function declarations" $
        aggregate 
            [ basicFuncDecl
            , basicTypeDecl
            , basicCplxDecl
            , Type "bar" [Bool, Int]
            , Cplx "bar" (Var "m")
            , Func "baz" ["n"] basicFuncDefn
            , Cplx "baz" (Op Exp (Lit (LInt 2)) (Var "q"))
            ]
            @?= Right (Map.fromList
                [ ("foo", (Just (Var "n"), Just [Int, Int], Just (["n"], basicFuncDefn)))
                , ("bar", (Just (Var "m"), Just [Bool, Int], Nothing))
                , ("baz", (Just (Op Exp (Lit (LInt 2)) (Var "q")), Nothing, Just (["n"], basicFuncDefn)))
                ])
    ]