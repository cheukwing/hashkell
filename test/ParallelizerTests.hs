module ParallelizerTests (parallelizerTests) where

import Parallelizer.Internal 
    (freeVariables
    , parseComplexity
    , complexityToBoundary
    , validateComplexityNames
    , buildInitFunctionTable
    , buildFunctionTable
    , ParallelizerError(..)
    , Complexity(..)
    , InitFunctionData(..)
    , FunctionData(..)
    )
import DependencyGraph.Internal ( DNode(..), DType(..), DExpr(..))
import Simple.Syntax

import Prelude hiding (GT, LT, EQ)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Control.Exception (try)
import Data.Either (Either(..), either)

import Test.Tasty
import Test.Tasty.HUnit

basicFuncDefn = Op Add (Lit (LInt 1)) (Lit (LInt 1))
basicFuncDecl = Func "foo" ["n"] basicFuncDefn
basicCplxDecl = Cplx "foo" (Var "n")
basicTypeDecl = Type "foo" [Int, Int]

basicFuncGraph = 
    ( Map.fromList 
        [ ("_", Scope)
        , ("_x0", Expression (DOp Add (DLit (LInt 1)) (DLit (LInt 1))))
        ]
    , Set.fromList [("_", "_x0", DepD)]
    )

parallelizerTests :: TestTree
parallelizerTests = testGroup "Parallelizer Tests" 
            [ freeVariablesTests
            , parseComplexityTests
            , complexityToBoundaryTests
            , validateComplexityNamesTests
            , buildInitFunctionTableTests
            , buildFunctionTableTests
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


parseComplexityTests = testGroup "parseComplexity tests"
    [ testCase "parses logarithmic time" $
        parseComplexity (App (Var "log") (Var "n"))
            @?= Right (Logarithmic "n")
    , testCase "parses linear time" $
        parseComplexity (Var "n")
            @?= Right (Polynomial "n" 1)
    , testCase "parses quadratic time" $
        parseComplexity (Op Exp (Var "n") (Lit (LInt 2)))
            @?= Right (Polynomial "n" 2)
    , testCase "parses exponential time" $
        parseComplexity (Op Exp (Lit (LInt 2)) (Var "n") )
            @?= Right (Exponential 2 "n")
    , testCase "parses constant time" $
        parseComplexity (Lit (LInt 100))
            @?= Right (Constant 100)
    , testCase "does not parse if" $
        parseComplexity (If (Op GT (Var "n") (Lit (LInt 10))) (Var "n") (Lit (LInt 100)))
            @?= Left IllegalComplexityAnnotation
    , testCase "does not parse let" $
        parseComplexity (Let [Def "m" (Lit (LInt 2))] (Op Exp (Var "n") (Var "m")))
            @?= Left IllegalComplexityAnnotation
    , testCase "does not support log applied to non-var" $
        parseComplexity (App (Var "log") (Lit (LInt 16)))
            @?= Left UnsupportedComplexityAnnotation
    , testCase "does not parse arbitrary function applications" $
        parseComplexity (App (Var "frog") (Var "n"))
            @?= Left IllegalComplexityAnnotation
    , testCase "does not parse arbitrary literal" $
        parseComplexity (Lit (LBool True))
            @?= Left IllegalComplexityAnnotation
    , testCase "does not support arbitrary operations" $
        parseComplexity (Op Exp (Var "n") (Var "m"))
            @?= Left UnsupportedComplexityAnnotation
    ]

complexityToBoundaryTests = testGroup "complexityToBoundary tests"
    [ testCase "does not create boundary for bool names" $
        complexityToBoundary (Polynomial "n" 2) Bool 100
            @?= Left IncompatibleComplexityAnnotation
    , testCase "creates simple boundary for constant time" $
        complexityToBoundary (Constant 100) Int 100
            @?= Right (Lit (LBool False))
    , testCase "creates boundary for linear time" $
        complexityToBoundary (Polynomial "n" 1) Int 100
            @?= Right (Op LT (Var "n") (Lit (LInt 100)))
    , testCase "creates boundary for quadratic time" $
        complexityToBoundary (Polynomial "n" 2) Int 100
            @?= Right (Op LT (Var "n") (Lit (LInt 10)))
    , testCase "creates boundary for exponential time" $
        complexityToBoundary (Exponential 2 "n") Int 1000000
            @?= Right (Op LT (Var "n") (Lit (LInt 20)))
    , testCase "creates simple boundary for logarithmic time" $
        complexityToBoundary (Logarithmic "n") Int 100
            @?= Right (Lit (LBool True))
    ]

validateComplexityNamesTests = testGroup "validateComplexityNames tests"
    [ testCase "fails for polynomial complexity with name not in args" $
        validateComplexityNames (Polynomial "n" 100) ["a", "b", "c"]
            @?= Left IncompatibleComplexityAnnotation
    , testCase "succeeds for polynomial complexity with name in args" $
        validateComplexityNames (Polynomial "b" 100) ["a", "b", "c"]
            @?= Right ()
    , testCase "succeeds for constant complexity" $
        validateComplexityNames (Constant 100000) ["z", "a"] 
            @?= Right ()
    ]

buildInitFunctionTableTests = testGroup "buildInitFunctionTable tests"
    [ testCase "forms complete when given cplx, type, func" $
        buildInitFunctionTable [basicCplxDecl, basicTypeDecl, basicFuncDecl]
            @?= Right (Map.fromList [("foo", Complete (Var "n") [Int, Int] ["n"] basicFuncDefn)])
    , testCase "forms complete when given type, cplx, func" $
        buildInitFunctionTable [basicTypeDecl, basicCplxDecl, basicFuncDecl]
            @?= Right (Map.fromList [("foo", Complete (Var "n") [Int, Int] ["n"] basicFuncDefn)])
    , testCase "forms complete when given func, cplx, type" $
        buildInitFunctionTable [basicFuncDecl, basicCplxDecl, basicTypeDecl]
            @?= Right (Map.fromList [("foo", Complete (Var "n") [Int, Int] ["n"] basicFuncDefn)])
    , testCase "forms partial when given func, type" $
        buildInitFunctionTable [basicFuncDecl, basicTypeDecl]
            @?= Right (Map.fromList [("foo", TypeDefinition [Int, Int] ["n"] basicFuncDefn)])
    , testCase "forms partial when given cplx, type" $
        buildInitFunctionTable [basicCplxDecl, basicTypeDecl]
            @?= Right (Map.fromList [("foo", ComplexityType (Var "n") [Int, Int])])
    , testCase "forms only func when given func" $
        buildInitFunctionTable [basicFuncDecl]
            @?= Right (Map.fromList [("foo", Definition ["n"] basicFuncDefn)])
    , testCase "fails when duplicate declarations present" $
        buildInitFunctionTable [basicFuncDecl, basicTypeDecl, Type "foo" [Int, Bool]]
            @?= Left DuplicateDeclaration
    , testCase "forms multiple entries for different function declarations" $
        buildInitFunctionTable 
            [ basicFuncDecl
            , basicTypeDecl
            , basicCplxDecl
            , Type "bar" [Bool, Int]
            , Cplx "bar" (Var "m")
            , Func "baz" ["n"] basicFuncDefn
            , Cplx "baz" (Op Exp (Lit (LInt 2)) (Var "q"))
            ]
            @?= Right (Map.fromList
                [ ("foo", Complete (Var "n") [Int, Int] ["n"] basicFuncDefn)
                , ("bar", ComplexityType (Var "m") [Bool, Int])
                , ("baz", ComplexityDefinition (Op Exp (Lit (LInt 2)) (Var "q")) ["n"] basicFuncDefn)
                ])
    ]

buildFunctionTableTests = testGroup "buildFunctionTable tests"
    [ testCase "ignores functions without definitions" $
        buildFunctionTable 100 (Map.fromList
        [ ("foo", Complexity (Var "m"))
        , ("bar", ComplexityType (Var "q") [Int, Int])
        , ("baz", TypeAnnotation [Bool, Int, Int, Int, Int, Int])
        ])
        @?= Right Map.empty
    , testCase "does not parallelise functions without complexity annotation" $
        buildFunctionTable 100 (Map.fromList
        [ ("foo", TypeDefinition [Int, Int] ["n"] basicFuncDefn)
        , ("bar", Definition ["n"] basicFuncDefn)
        ])
        @?= Right (Map.fromList 
        [ ("foo", SequentialT [Int, Int] ["n"] basicFuncDefn)
        , ("bar", Sequential ["n"] basicFuncDefn)
        ])
    , testCase "does not parallelise functions with low trivial complexity" $
        buildFunctionTable 100 (Map.fromList
        [ ("foo", Complete (Lit (LInt 50)) [Int, Int] ["n"] basicFuncDefn)
        , ("bar", ComplexityDefinition (Lit (LInt 99)) ["n"] basicFuncDefn)
        , ("baz", ComplexityDefinition (App (Var "log") (Var "n")) ["n"] basicFuncDefn)
        ])
        @?= Right (Map.fromList 
        [ ("foo", SequentialT [Int, Int] ["n"] basicFuncDefn)
        , ("bar", Sequential ["n"] basicFuncDefn)
        , ("baz", Sequential ["n"] basicFuncDefn)
        ])
    , testCase "exclusively parallelises functions with high trivial complexity" $
        buildFunctionTable 100 (Map.fromList
        [ ("foo", Complete (Lit (LInt 100)) [Int, Int] ["n"] basicFuncDefn)
        , ("bar", ComplexityDefinition (Lit (LInt 10000000)) ["n"] basicFuncDefn)
        ])
        @?= Right (Map.fromList 
        [ ("foo", ParallelT [Int, Int] ["n"] basicFuncGraph)
        , ("bar", Parallel ["n"] basicFuncGraph)
        ])
    , testCase "parallelise functions with polynomial complexity" $
        buildFunctionTable 100 (Map.fromList
        [ ("foo", Complete (Op Exp (Var "n") (Lit (LInt 2))) [Int, Int] ["n"] basicFuncDefn)
        , ("bar", ComplexityDefinition (Var "n") ["n"] basicFuncDefn)
        ])
        @?= Right (Map.fromList 
        [ ("foo", SequentialT [Int, Int] ["n"] 
                    (If (Op LT (Var "n") (Lit (LInt 10))) 
                        (App (Var "foo_seq") (Var "n")) 
                        (App (Var "foo_par") (Var "n"))
                    ))
        , ("foo_seq", SequentialT [Int, Int] ["n"] basicFuncDefn)
        , ("foo_par", ParallelT [Int, Int] ["n"] basicFuncGraph)
        , ("bar", Sequential ["n"] 
                    (If (Op LT (Var "n") (Lit (LInt 100))) 
                        (App (Var "bar_seq") (Var "n")) 
                        (App (Var "bar_par") (Var "n"))
                    ))
        , ("bar_seq", Sequential ["n"] basicFuncDefn)
        , ("bar_par", Parallel ["n"] basicFuncGraph)
        ])
    , testCase "fails when complexity is incompatible with function" $
        buildFunctionTable 100 (Map.fromList
        [ ("foo", Complete (Op Exp (Var "n") (Lit (LInt 2))) [Int, Int] ["m"] basicFuncDefn)
        ])
        @?= Left IncompatibleComplexityAnnotation
    , testCase "fails when complexity is incompatible with types" $
        buildFunctionTable 100 (Map.fromList
        [ ("foo", Complete (Op Exp (Var "n") (Lit (LInt 2))) [Bool, Int] ["n"] basicFuncDefn)
        ])
        @?= Left IncompatibleComplexityAnnotation
    , testCase "fails when complexity is uses illegal expressions" $
        buildFunctionTable 100 (Map.fromList
        [ ("foo", Complete 
            (If (Lit (LBool True)) (Var "n") (Op Exp (Var "n") (Lit (LInt 2))))
            [Int, Int] 
            ["n"] 
            basicFuncDefn)
        ])
        @?= Left IllegalComplexityAnnotation
    , testCase "fails when complexity is uses unsupported expressions" $
        buildFunctionTable 100 (Map.fromList
        [ ("foo", Complete 
            (Op Add (Var "n") (Var "m"))
            [Int, Int, Int] 
            ["n", "m"] 
            basicFuncDefn)
        ])
        @?= Left UnsupportedComplexityAnnotation
    ]