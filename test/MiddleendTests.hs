module MiddleendTests (middleendTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Prelude hiding (EQ, LT, GT)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Context
import Hashkell.Syntax
import Frontend (Cplx(..))
import Middleend.Cleaner (ensureUniqueNames, ensureNoUnusedDefs)
import Middleend.Paralleliser 
    ( parallelisationType
    , ParallelisationType(..)
    , EncodingInstruction(..)
    , createEncodingInstructionTable
    , hasParallelism
    )
import Middleend.DependencyGraph (DType(..), DNode(..), DLit(..), DExpr(..), createDependencyGraph)

testContext = Context { boundarySteps = 100, fewerAtomicNodes = True, noRedundantArcs = True }

_createDependencyGraph = createDependencyGraph testContext Map.empty

_createEncodingInstructionTable = createEncodingInstructionTable testContext

middleendTests :: TestTree
middleendTests = testGroup "Middleend Tests"
    [ ensureUniqueNamesTests
    , ensureNoUnusedDefsTests
    , parallelisationTypeTests
    , createEncodingInstructionTableTests
    , hasParallelismTests
    , createDependencyGraphTests
    ]

_ensureUniqueNamesJustExpr e = e'
    where (_, _, e') = ensureUniqueNames Nothing [] e

_ensureUniqueNamesWithParams ps e = (ps', e')
    where (_, ps', e') = ensureUniqueNames Nothing ps e

ensureUniqueNamesTests = testGroup "ensureUniqueNames tests"
    [ testCase "expressions with unique names are unchanged" $
        _ensureUniqueNamesJustExpr
            (Let [ Def "x" (Lit (LInt 1))
                 , Def "y" (Lit (LInt 2))
                 ] 
                 (Let [ Def "z" (Lit (LInt 3))
                      , Def "w" (Lit (LInt 4))
                      ] (Var "x")))
            @?=
            Let [ Def "x" (Lit (LInt 1))
                , Def "y" (Lit (LInt 2))
                ] 
                (Let [ Def "z" (Lit (LInt 3))
                     , Def "w" (Lit (LInt 4))
                     ] (Var "x"))
    , testCase "definitions with non-unique names are changed" $
        _ensureUniqueNamesJustExpr
            (Let [ Def "x" (Lit (LInt 1))
                 , Def "y" (Lit (LInt 2))
                 ] 
                 (Let [ Def "y" (Lit (LInt 3))
                      , Def "z" (Lit (LInt 4))
                      ] (Var "x")))
            @?=
            Let [ Def "x" (Lit (LInt 1))
                , Def "y" (Lit (LInt 2))
                ] 
                (Let [ Def "_y0" (Lit (LInt 3))
                     , Def "z" (Lit (LInt 4))
                     ] (Var "x"))
    , testCase "definitions with non-unique names and their users are changed" $
        _ensureUniqueNamesJustExpr
            (Let [ Def "x" (Lit (LInt 1))
                 , Def "y" (Lit (LInt 2))
                 ] 
                 (Let [ Def "y" (Lit (LInt 3))
                      , Def "z" (Lit (LInt 4))
                      ] (Var "y")))
            @?=
            Let [ Def "x" (Lit (LInt 1))
                , Def "y" (Lit (LInt 2))
                ] 
                (Let [ Def "_y0" (Lit (LInt 3))
                     , Def "z" (Lit (LInt 4))
                     ] (Var "_y0"))
    , testCase "does not change users of non-unique names in a different scope" $
        _ensureUniqueNamesJustExpr
            (Let [ Def "x" (Let [Def "x" (Lit (LInt 1))] (Var "x")) ] 
                (Op Add (Var "x") (Lit (LInt 2))))
            @?=
            Let [ Def "x" (Let [Def "_y0" (Lit (LInt 1))] (Var "_y0")) ] 
                (Op Add (Var "x") (Lit (LInt 2)))
    , testCase "changes names which are similar to those generated by the application" $
        _ensureUniqueNamesJustExpr
            (Let [ Def "_x0" (Lit (LInt 1))
                 , Def "_y0" (Lit (LInt 2))
                 ] 
                 (Let [ Def "_x1" (Lit (LInt 3))
                      , Def "_x2" (Lit (LInt 4))
                      ] (Var "_y0")))
            @?=
            Let [ Def "_y0" (Lit (LInt 1))
                , Def "_y1" (Lit (LInt 2))
                ] 
                (Let [ Def "_y2" (Lit (LInt 3))
                     , Def "_y3" (Lit (LInt 4))
                     ] (Var "_y1"))
    , testCase "changes names inside other expressions" $
        _ensureUniqueNamesJustExpr
            (Op Add (Var "n") (Let [ Def "x" (Let [Def "x" (Lit (LInt 1))] (Var "x")) ] 
                (Op Add (Var "x") (Lit (LInt 2)))))
            @?=
            Op Add (Var "n") (Let [ Def "x" (Let [Def "_y0" (Lit (LInt 1))] (Var "_y0")) ] 
                (Op Add (Var "x") (Lit (LInt 2))))
    , testCase "changes names which are the same as the params" $
        _ensureUniqueNamesWithParams ["n", "m"]
            (Let [ Def "n" (Lit (LInt 1))
                 , Def "m" (Lit (LInt 2))
                 ] 
                 (Let [ Def "n" (Lit (LInt 3))
                      , Def "m" (Lit (LInt 4))
                      ] (Op Add (Var "n") (Var "m"))))
            @?=
            (["n", "m"]
            , Let [ Def "_y0" (Lit (LInt 1))
                , Def "_y1" (Lit (LInt 2))
                ] 
                (Let [ Def "_y2" (Lit (LInt 3))
                     , Def "_y3" (Lit (LInt 4))
                     ] (Op Add (Var "_y2") (Var "_y3")))
            )
    , testCase "changes param names which are similar to generated names" $
        _ensureUniqueNamesWithParams ["_x", "_y"] (Lit (LInt 1))
            @?=
            (["_y0", "_y1"], Lit (LInt 1))
    , testCase "changes param names which are similar to generated names and their users" $
        _ensureUniqueNamesWithParams ["_x", "_y"] 
            (Let [Def "_x" (Lit (LInt 1))] (Op Add (Var "_x") (Var "_y")))
            @?=
            (["_y0", "_y1"], Let [Def "_y2" (Lit (LInt 1))] (Op Add (Var "_y2") (Var "_y1")))
    , testCase "changes only param names which are similar to generated names and their users" $
        _ensureUniqueNamesWithParams ["x", "_x", "_y", "y", "_y1"] 
            (Let [Def "_x" (Lit (LInt 1))] (Op Add (Var "_x") (Var "_y")))
            @?=
            (["x", "_y0", "_y1", "y", "_y2"], Let [Def "_y3" (Lit (LInt 1))] (Op Add (Var "_y3") (Var "_y1")))
    , testCase "changes complexity with param names which are similar to generated names" $
        ensureUniqueNames (Just (Polynomial "_x" 2)) ["_x"] (Lit (LInt 1))
            @?=
            (Just (Polynomial "_y0" 2), ["_y0"], Lit (LInt 1))
    , testCase "makes no changes in complexity with param names which are not similar to generated names" $
        ensureUniqueNames (Just (Polynomial "x" 2)) ["x"] (Lit (LInt 1))
            @?=
            (Just (Polynomial "x" 2), ["x"], Lit (LInt 1))
    ]

ensureNoUnusedDefsTests = testGroup "ensureNoUsedDefs tests"
    [ testCase "expressions with no unused defs are unchanged" $
        ensureNoUnusedDefs 
            (Let [ Def "x" (Lit (LInt 1))
                 , Def "y" (Lit (LInt 2))
                 ] 
                 (Let [ Def "z" (Lit (LInt 3))
                      , Def "w" (Lit (LInt 4))
                      ] (Op Add (Var "x") (Op Add (Var "y") (Op Add (Var "z") (Var "w"))))))
            @?=
            Let [ Def "x" (Lit (LInt 1))
                , Def "y" (Lit (LInt 2))
                ] 
                (Let [ Def "z" (Lit (LInt 3))
                     , Def "w" (Lit (LInt 4))
                     ] (Op Add (Var "x") (Op Add (Var "y") (Op Add (Var "z") (Var "w")))))
    , testCase "removes unused defs" $
        ensureNoUnusedDefs 
            (Let [ Def "x" (Lit (LInt 1))
                 , Def "y" (Lit (LInt 2))
                 ] 
                 (Let [ Def "z" (Lit (LInt 3))
                      , Def "w" (Lit (LInt 4))
                      ] (Op Add (Var "x") (Var "w"))))
            @?=
            Let [ Def "x" (Lit (LInt 1)) ] 
                (Let [ Def "w" (Lit (LInt 4)) ]
                    (Op Add (Var "x") (Var "w")))
    , testCase "removes let expressions if all expressions unused" $
        ensureNoUnusedDefs 
            (Let [ Def "x" (Lit (LInt 1))
                 , Def "y" (Lit (LInt 2))
                 ] 
                 (Let [ Def "z" (Lit (LInt 3))
                      , Def "w" (Lit (LInt 4))
                      ] (Lit (LInt 0))))
            @?= Lit (LInt 0)
    , testCase "removes unused defs which are used by other unuseddefs" $
        ensureNoUnusedDefs
            (Let [ Def "x" (Lit (LInt 1))
                 , Def "y" (Var "x")
                 , Def "z" (Var "y")]
                 (Let [ Def "w" (Var "z")] (Lit (LInt 2))))
            @?= Lit (LInt 2)
    , testCase "removes unused defs which are recursive" $
        ensureNoUnusedDefs
            (Let [ Def "x" (Var "x")
                 , Def "y" (Op Add (Var "x") (Var "y"))]
                 (Lit (LInt 2)))
            @?= Lit (LInt 2)
    ]

defnWithBranch
    = Op Add (App (Var "meep") (Lit (LInt 1))) (App (Var "beep") (Lit (LInt 2)))
functionWithBranch
    = Just (["n"], defnWithBranch)
graphWithBranch
    = ( Map.fromList 
        [ ("_", Scope)
        , ("_x1", Expression (DApp "meep" [DLit (DInt 1)]))
        , ("_x2", Expression (DApp "beep" [DLit (DInt 2)]))
        , ("_x0", Expression (DOp Add (DVar "_x1") (DVar "_x2")))
        ]
      , Set.fromList
        [ ("_", "_x1", Dep)
        , ("_", "_x2", Dep)
        , ("_x1", "_x0", Dep)
        , ("_x2", "_x0", Dep)
        ]
      )

parallelisationTypeTests = testGroup "parallelisationType tests"
    [ testCase "never parallelise a function with no complexity annotation" $
        parallelisationType 1000 (Nothing, Just [Int, Int], functionWithBranch)
            @?= Never
    , testCase "never parallelise a function with no defn" $
        parallelisationType 1000 (Just (Polynomial "n" 2), Just [Int, Int], Nothing)
            @?= Never
    , testCase "always parallelise a function with constant time greater than steps" $
        parallelisationType 1000 (Just (Constant 1001), Just [Int, Int], functionWithBranch)
            @?= Always
    , testCase "never parallelise a function with constant time lower than steps" $
        parallelisationType 1000 (Just (Constant 999), Just [Int, Int], functionWithBranch)
            @?= Never
    , testCase "never parallelise a function with logarithmic time" $
        parallelisationType 1000 (Just (Logarithmic "n"), Nothing, functionWithBranch)
            @?= Never
    , testCase "never parallelise a function with logarithmic time" $
        parallelisationType 1000 (Just (Factorial "n"), Nothing, functionWithBranch)
            @?= Branching (Op GTE (Var "n") (Lit (LInt 7)))
    , testCase "branching parallelisation for linear time" $
        parallelisationType 100 (Just (Polynomial "n" 1), Nothing, functionWithBranch)
            @?= Branching (Op GTE (Var "n") (Lit (LInt 100)))
    , testCase "branching parallelisation for quadratic time" $
        parallelisationType 100 (Just (Polynomial "n" 2), Nothing, functionWithBranch)
            @?= Branching (Op GTE (Var "n") (Lit (LInt 10)))
    , testCase "branching parallelisation for exponential time" $
        parallelisationType 1000000 (Just (Exponential 2 "n"), Nothing, functionWithBranch)
            @?= Branching (Op GTE (Var "n") (Lit (LInt 20)))
    , testCase "correct lhs for boundary in branching parallelisation for list type" $
        parallelisationType 100 (Just (Polynomial "n" 2), Just [List Int, Int], functionWithBranch)
            @?= Branching (Op GTE (App (Var "length") (Var "n")) (Lit (LInt 10)))
    ]

createEncodingInstructionTableTests = testGroup "_createEncodingInstructionTable tests"
    [ testCase "ignores functions without definitions" $
        _createEncodingInstructionTable (Map.fromList
        [ ("foo", (Just (Polynomial "m" 1), Nothing, Nothing))
        , ("bar", (Just (Polynomial "q" 1), Just [Int, Int], Nothing))
        , ("baz", (Nothing, Just [Bool, Int, Int, Int, Int, Int], Nothing))
        ])
        @?= Map.empty
    , testCase "does not parallelise functions without complexity annotation" $
        _createEncodingInstructionTable (Map.fromList
        [ ("foo", (Nothing, Just [Int, Int], functionWithBranch))
        , ("bar", (Nothing, Nothing, functionWithBranch))
        ])
        @?= Map.fromList 
        [ ("foo", Sequential (Just [Int, Int]) ["n"] defnWithBranch)
        , ("bar", Sequential Nothing ["n"] defnWithBranch)
        ]
    , testCase "does not parallelise functions with low complexity" $
        _createEncodingInstructionTable (Map.fromList
        [ ("foo", (Just (Constant 50), Just [Int, Int], functionWithBranch))
        , ("bar", (Just (Constant 99), Nothing, functionWithBranch))
        , ("baz", (Just (Logarithmic "n"), Nothing, functionWithBranch))
        ])
        @?= Map.fromList 
        [ ("foo", Sequential (Just [Int, Int]) ["n"] defnWithBranch)
        , ("bar", Sequential Nothing ["n"] defnWithBranch)
        , ("baz", Sequential Nothing ["n"] defnWithBranch)
        ]
    , testCase "exclusively parallelises functions with high trivial complexity" $
        _createEncodingInstructionTable (Map.fromList
        [ ("foo", (Just (Constant 101), Just [Int, Int], functionWithBranch))
        , ("bar", (Just (Constant 10000000), Nothing, functionWithBranch))
        ])
        @?= Map.fromList 
        [ ("foo", Parallel (Just [Int, Int]) ["n"] graphWithBranch)
        , ("bar", Parallel Nothing ["n"] graphWithBranch)
        ]
    , testCase "parallelise functions with polynomial complexity" $
        _createEncodingInstructionTable (Map.fromList
        [ ("foo", (Just (Polynomial "n" 2), Just [Int, Int], functionWithBranch))
        , ("bar", (Just (Polynomial "n" 1), Nothing, functionWithBranch))
        ])
        @?= Map.fromList 
        [ ("foo", Sequential (Just [Int, Int]) ["n"] 
                    (If (Op GTE (Var "n") (Lit (LInt 10))) 
                        (App (Var "foo_par") (Var "n"))
                        (App (Var "foo_seq") (Var "n")) 
                    ))
        , ("foo_seq", Sequential (Just [Int, Int]) ["n"] defnWithBranch)
        , ("foo_par", Parallel (Just [Int, Int]) ["n"] graphWithBranch)
        , ("bar", Sequential Nothing ["n"] 
                    (If (Op GTE (Var "n") (Lit (LInt 100))) 
                        (App (Var "bar_par") (Var "n"))
                        (App (Var "bar_seq") (Var "n")) 
                    ))
        , ("bar_seq", Sequential Nothing ["n"] defnWithBranch)
        , ("bar_par", Parallel Nothing ["n"] graphWithBranch)
        ]
    , testCase "replaces recursive calls with the sequential branch, in the sequential branch" $
        _createEncodingInstructionTable (Map.fromList
        [ ( "foo"
          , ( Just (Polynomial "n" 2)
            , Just [Int, Int]
            , Just (["n"], Op Add 
                (App (Var "foo") (Lit (LInt 1))) 
                (App (Var "beep") (Lit (LInt 2))))
            ) 
          )
        ])
        @?= Map.fromList 
        [ ("foo", Sequential (Just [Int, Int]) ["n"] 
                    (If (Op GTE (Var "n") (Lit (LInt 10))) 
                        (App (Var "foo_par") (Var "n"))
                        (App (Var "foo_seq") (Var "n")) 
                    ))
        , ("foo_seq", Sequential (Just [Int, Int]) 
                        ["n"] 
                        (Op Add 
                            (App (Var "foo_seq") (Lit (LInt 1))) 
                            (App (Var "beep") (Lit (LInt 2)))))
        , ( "foo_par"
          , Parallel (Just [Int, Int])
                ["n"]
                ( Map.fromList 
                    [ ("_", Scope)
                    , ("_x1", Expression (DApp "foo" [DLit (DInt 1)]))
                    , ("_x2", Expression (DApp "beep" [DLit (DInt 2)]))
                    , ("_x0", Expression (DOp Add (DVar "_x1") (DVar "_x2")))
                    ]
                , Set.fromList
                    [ ("_", "_x1", Dep)
                    , ("_", "_x2", Dep)
                    , ("_x1", "_x0", Dep)
                    , ("_x2", "_x0", Dep)
                    ]
                )
          )
        ]
    ]


hasParallelismTests = testGroup "hasParallelism tests"
    [ testCase "returns false in one node graph" $
        hasParallelism
            ( Map.fromList
                [ ("_", Scope)
                , ("_x0", Expression (DOp Add (DLit (DInt 1)) (DLit (DInt 1))))
                ]
            , Set.fromList
                [ ("_", "_x0", Dep)
                ]
            )
        @?= False
    , testCase "returns false in simple sequential graph" $
        hasParallelism
            ( Map.fromList
                [ ("_", Scope)
                , ("_x1", Expression (DApp "foo" [DLit (DInt 1)]))
                , ("_x0", Expression (DOp Add (DVar "_x1") (DLit (DInt 1))))
                ]
            , Set.fromList
                [ ("_", "_x1", Dep)
                , ("_x1", "_x0", Dep)
                ]
            )
        @?= False
    , testCase "returns false in long sequential graph" $
        hasParallelism
            ( Map.fromList
                [ ("_", Scope)
                , ("_x7", Expression (DApp "foo" [DVar "_x8"]))
                , ("_x6", Expression (DApp "foo" [DVar "_x7"]))
                , ("_x5", Expression (DApp "foo" [DVar "_x6"]))
                , ("_x4", Expression (DApp "foo" [DVar "_x5"]))
                , ("_x3", Expression (DApp "foo" [DVar "_x4"]))
                , ("_x2", Expression (DApp "foo" [DVar "_x3"]))
                , ("_x1", Expression (DApp "foo" [DVar "_x2"]))
                , ("_x0", Expression (DOp Add (DVar "_x1") (DLit (DInt 1))))
                ]
            , Set.fromList
                [ ("_", "_x7", Dep)
                , ("_x7", "_x6", Dep)
                , ("_x6", "_x5", Dep)
                , ("_x5", "_x4", Dep)
                , ("_x4", "_x3", Dep)
                , ("_x3", "_x2", Dep)
                , ("_x2", "_x1", Dep)
                , ("_x1", "_x0", Dep)
                ]
            )
        @?= False
    , testCase "returns false for sequential graph with condition" $
        hasParallelism
            ( Map.fromList
                [ ("_", Scope)
                , ("_x0", Conditional (DVar "_x1"))
                , ("_x1", Expression (DApp "foo" [DLit (DInt 1)]))
                , ("_2", Scope)
                , ("_x3", Expression (DOp Add (DVar "_x4") (DLit (DInt 1))))
                , ("_x4", Expression (DApp "bar" [DLit (DInt 1)]))
                , ("_5", Scope)
                , ("_x6", Expression (DOp Add (DVar "_x7") (DLit (DInt 1))))
                , ("_x7", Expression (DApp "baz" [DLit (DInt 1)]))
                ]
            , Set.fromList
                [ ("_", "_x1", Dep)
                , ("_x1", "_x0", Dep)
                , ("_x0", "_2", DepThen)
                , ("_2", "_x4", Dep)
                , ("_x4", "_x3", Dep)
                , ("_x0", "_5", DepElse)
                , ("_5", "_x7", Dep)
                , ("_x7", "_x6", Dep)
                ]
            )
        @?= False
    , testCase "returns false for sequential graph with condition as expression" $
        hasParallelism
            ( Map.fromList
                [ ("_", Scope)
                , ("_x0", Conditional (DVar "_x1"))
                , ("_x1", Expression (DApp "foo" [DLit (DInt 1)]))
                , ("_2", Scope)
                , ("_x3", Expression (DOp Add (DVar "_x4") (DLit (DInt 1))))
                , ("_x4", Expression (DApp "bar" [DLit (DInt 1)]))
                , ("_5", Scope)
                , ("_x6", Expression (DOp Add (DVar "_x7") (DLit (DInt 1))))
                , ("_x7", Expression (DApp "baz" [DLit (DInt 1)]))
                , ("_x8", Expression (DOp Add (DLit (DInt 1)) (DVar "_x0")))
                ]
            , Set.fromList
                [ ("_", "_x1", Dep)
                , ("_x1", "_x0", Dep)
                , ("_x0", "_2", DepThen)
                , ("_2", "_x4", Dep)
                , ("_x4", "_x3", Dep)
                , ("_x0", "_5", DepElse)
                , ("_5", "_x7", Dep)
                , ("_x7", "_x6", Dep)
                , ("_x0", "_x8", Dep)
                ]
            )
        @?= False
    , testCase "returns true for simple graph with branch" $
        hasParallelism graphWithBranch
        @?= True
    , testCase "returns true for graph with parallelism before conditional" $
        hasParallelism
            ( Map.fromList
                [ ("_", Scope)
                , ("_x0", Conditional (DVar "_x1"))
                , ("_x1", Expression (DApp "foo" [DVar "a", DVar "b"]))
                , ("a", Expression (DApp "bionics" [DLit (DInt 1)]))
                , ("b", Expression (DApp "tronics" [DLit (DInt 1)]))
                , ("_2", Scope)
                , ("_x3", Expression (DOp Add (DVar "_x4") (DLit (DInt 1))))
                , ("_x4", Expression (DApp "bar" [DLit (DInt 1)]))
                , ("_5", Scope)
                , ("_x6", Expression (DOp Add (DVar "_x7") (DLit (DInt 1))))
                , ("_x7", Expression (DApp "baz" [DLit (DInt 1)]))
                ]
            , Set.fromList
                [ ("_", "a", Dep)
                , ("_", "b", Dep)
                , ("a", "_x1", Dep)
                , ("b", "_x1", Dep)
                , ("_x1", "_x0", Dep)
                , ("_x0", "_2", DepThen)
                , ("_2", "_x4", Dep)
                , ("_x4", "_x3", Dep)
                , ("_x0", "_5", DepElse)
                , ("_5", "_x7", Dep)
                , ("_x7", "_x6", Dep)
                ]
            )
        @?= True
    , testCase "returns true for graph with parallelism inside one scope" $
        hasParallelism
            ( Map.fromList
                [ ("_", Scope)
                , ("_x0", Conditional (DVar "_x1"))
                , ("_x1", Expression (DApp "foo" [DLit (DInt 1)]))
                , ("_2", Scope)
                , ("_x3", Expression (DOp Add (DVar "_x4") (DLit (DInt 1))))
                , ("_x4", Expression (DApp "bar" [DLit (DInt 1)]))
                , ("_5", Scope)
                , ("_x6", Expression (DOp Add (DVar "_x7") (DVar "_x8")))
                , ("_x7", Expression (DApp "baz" [DLit (DInt 1)]))
                , ("_x8", Expression (DApp "baz" [DLit (DInt 1)]))
                ]
            , Set.fromList
                [ ("_", "_x1", Dep)
                , ("_x1", "_x0", Dep)
                , ("_x0", "_2", DepThen)
                , ("_2", "_x4", Dep)
                , ("_x4", "_x3", Dep)
                , ("_x0", "_5", DepElse)
                , ("_5", "_x7", Dep)
                , ("_5", "_x8", Dep)
                , ("_x7", "_x6", Dep)
                , ("_x8", "_x6", Dep)
                ]
            )
        @?= True
    , testCase "returns true for graph with parallelism inside both scopes" $
        hasParallelism
            ( Map.fromList
                [ ("_", Scope)
                , ("_x0", Conditional (DVar "_x1"))
                , ("_x1", Expression (DApp "foo" [DLit (DInt 1)]))
                , ("_2", Scope)
                , ("_x3", Expression (DOp Add (DVar "_x4") (DVar "_x5")))
                , ("_x4", Expression (DApp "bar" [DLit (DInt 1)]))
                , ("_x5", Expression (DApp "bar" [DLit (DInt 1)]))
                , ("_6", Scope)
                , ("_x7", Expression (DOp Add (DVar "_x8") (DVar "_x9")))
                , ("_x8", Expression (DApp "baz" [DLit (DInt 1)]))
                , ("_x9", Expression (DApp "baz" [DLit (DInt 1)]))
                ]
            , Set.fromList
                [ ("_", "_x1", Dep)
                , ("_x1", "_x0", Dep)
                , ("_x0", "_2", DepThen)
                , ("_2", "_x4", Dep)
                , ("_2", "_x5", Dep)
                , ("_x5", "_x3", Dep)
                , ("_x4", "_x3", Dep)
                , ("_x0", "_6", DepElse)
                , ("_6", "_x8", Dep)
                , ("_6", "_x9", Dep)
                , ("_x8", "_x7", Dep)
                , ("_x9", "_x7", Dep)
                ]
            )
        @?= True
    , testCase "returns true for parallel graph with condition as expression" $
        hasParallelism
            ( Map.fromList
                [ ("_", Scope)
                , ("_x0", Conditional (DVar "_x1"))
                , ("_x1", Expression (DApp "foo" [DLit (DInt 1)]))
                , ("_2", Scope)
                , ("_x3", Expression (DOp Add (DVar "_x4") (DLit (DInt 1))))
                , ("_x4", Expression (DApp "bar" [DLit (DInt 1)]))
                , ("_5", Scope)
                , ("_x6", Expression (DOp Add (DVar "_x7") (DLit (DInt 1))))
                , ("_x7", Expression (DApp "baz" [DLit (DInt 1)]))
                , ("_x8", Expression (DOp Add (DVar "_x9") (DVar "_x0")))
                , ("_x9", Expression (DApp "bong" [DLit (DInt 1)]))
                ]
            , Set.fromList
                [ ("_", "_x1", Dep)
                , ("_x1", "_x0", Dep)
                , ("_x0", "_2", DepThen)
                , ("_2", "_x4", Dep)
                , ("_x4", "_x3", Dep)
                , ("_x0", "_5", DepElse)
                , ("_5", "_x7", Dep)
                , ("_x7", "_x6", Dep)
                , ("_x0", "_x8", Dep)
                , ("_", "_x9", Dep)
                , ("_x9", "_x8", Dep)
                ]
            )
        @?= True
    ]

createDependencyGraphTests = testGroup "_createDependencyGraph tests"
    [ testCase "create very basic graph" $
        _createDependencyGraph [] (Lit (LInt 1))
            @?= ( Map.fromList 
                    [ ("_", Scope)
                    , ("_x0", Expression (DLit (DInt 1)))
                    ]
                , Set.fromList [("_", "_x0", Dep)]
                )
    , testCase "create very basic graph with argument" $
        _createDependencyGraph ["n"] (Var "n")
            @?= ( Map.fromList 
                    [("_", Scope), ("_x0", Expression (DVar "n"))]
                , Set.fromList 
                    [ ("_", "_x0", Dep)
                    --, ("_", "_x0", DepParam)
                    ]
                )
    , testCase "create atomic binary operation graph" $
        _createDependencyGraph ["n"] (Op Add (Op Add (Lit (LInt 1)) (Lit (LInt 2))) (Lit (LInt 3)))
            @?= ( Map.fromList 
                    [ ("_", Scope)
                    , ("_x0", Expression (DOp Add (DOp Add (DLit (DInt 1)) (DLit (DInt 2))) (DLit (DInt 3))))
                    ]
                , Set.fromList [("_", "_x0", Dep)]
                )
    , testCase "create binary operation graph" $
        _createDependencyGraph ["n"] (Op Add (Var "n") (Lit (LInt 3)))
            @?= ( Map.fromList 
                    [ ("_", Scope)
                    , ("_x0", Expression (DOp Add (DVar "n") (DLit (DInt 3))))
                    ]
                , Set.fromList 
                    [ ("_", "_x0", Dep)
                    --, ("_", "_x0", DepParam)
                    ]
                )
    , testCase "create function application graph" $
        _createDependencyGraph ["n"] (App (App (Var "someFunc") (Var "n")) (Op Add (Lit (LInt 1)) (Lit (LInt 1))))
            @?= ( Map.fromList 
                    [ ("_", Scope)
                    , ("_x0", Expression (DApp "someFunc" [DVar "n", DOp Add (DLit (DInt 1)) (DLit (DInt 1))]))
                    ]
                , Set.fromList 
                    [ ("_", "_x0", Dep)
                    --, ("_", "_x0", DepParam)
                    ]
                )
    , testCase "create let graph" $
        _createDependencyGraph ["n"] 
            (Let [ Def "a" (Lit (LInt 1))
                 , Def "b" (Lit (LInt 2))
                 ] 
                 (Op Add (Var "a") (Var "n"))
            )
            @?= ( Map.fromList 
                    [ ("_", Scope)
                    , ("a", Expression (DLit (DInt 1)))
                    , ("b", Expression (DLit (DInt 2)))
                    , ("_x0", Expression (DOp Add (DVar "a") (DVar "n")))
                    ]
                , Set.fromList 
                    [ ("a", "_x0", Dep)
                    , ("_", "a", Dep)
                    , ("_", "b", Dep)
--                  , ("_", "_x0", DepParam)
                    ]
                )
    , testCase "create let graph with reversed expression order" $
        _createDependencyGraph ["n"] 
            (Let [ Def "a" (Lit (LInt 1))
                 , Def "b" (Lit (LInt 2))
                 ] 
                 (Op Add (Var "n") (Var "a"))
            )
            @?= ( Map.fromList 
                    [ ("_", Scope)
                    , ("a", Expression (DLit (DInt 1)))
                    , ("b", Expression (DLit (DInt 2)))
                    , ("_x0", Expression (DOp Add (DVar "n") (DVar "a")))
                    ]
                , Set.fromList 
                    [ ("a", "_x0", Dep)
                    , ("_", "a", Dep)
                    , ("_", "b", Dep)
                    ]
                )
    , testCase "create if graph" $
        _createDependencyGraph ["a", "b"] 
            (If (Op LT (Var "a") (Lit (LInt 1)))
                (Lit (LInt 2))
                (App (Var "foobar") (Op Add (Var "a") (Var "b")))
            )
            @?= ( Map.fromList 
                    [ ("_", Scope)
                    , ("_x0", Conditional (DOp LT (DVar "a") (DLit (DInt 1))))
                    , ("_1", Scope)
                    , ("_x2", Expression (DLit (DInt 2)))
                    , ("_3", Scope)
                    , ("_x4", Expression (DApp "foobar" [DOp Add (DVar "a") (DVar "b")]))
                    ]
                , Set.fromList 
                    [ ("_", "_x0", Dep)
--                    , ("_", "_x0", DepParam)
                    , ("_x0", "_1", DepThen)
                    , ("_x0", "_3", DepElse)
                    , ("_1", "_x2", Dep)
                    , ("_3", "_x4", Dep)
--                    , ("_", "_x4", DepParam)
                    ]
                )
    , testCase "rearrange external dependencies" $
        _createDependencyGraph []
            (Let [Def "a" (Lit (LInt 1))]
                 (If (Lit (LBool True)) (Var "a") (Lit (LInt 2))))
            @?= ( Map.fromList
                    [ ("_", Scope)
                    , ("a", Expression (DLit (DInt 1)))
                    , ("_x0", Conditional (DLit (DBool True)))
                    , ("_1", Scope)
                    , ("_x2", Expression (DVar "a"))
                    , ("_3", Scope)
                    , ("_x4", Expression (DLit (DInt 2)))
                    ]
                , Set.fromList
                    [ ("_", "a", Dep)
                    , ("a", "_x0", Dep)
                    , ("_x0", "_1", DepThen)
                    , ("_x0", "_3", DepElse)
                    , ("_1", "_x2", Dep)
                    , ("_3", "_x4", Dep)
                    ]
                )
    , testCase "rearrange nested external dependencies" $
        _createDependencyGraph []
            (Let [Def "a" (Lit (LInt 1))]
                 (If (Lit (LBool True)) 
                     (Let [Def "b" (Lit (LInt 2))]
                          (If (Lit (LBool True)) (Op Add (Var "a") (Var "b")) (Lit (LInt 3)))) 
                     (Lit (LInt 2))))
            @?= ( Map.fromList
                    [ ("_", Scope)
                    , ("a", Expression (DLit (DInt 1)))
                    , ("b", Expression (DLit (DInt 2)))
                    , ("_x0", Conditional (DLit (DBool True)))
                    , ("_1", Scope)
                    , ("_x2", Conditional (DLit (DBool True)))
                    , ("_3", Scope)
                    , ("_x4", Expression (DOp Add (DVar "a") (DVar "b")))
                    , ("_5", Scope)
                    , ("_x6", Expression (DLit (DInt 3)))
                    , ("_7", Scope)
                    , ("_x8", Expression (DLit (DInt 2)))
                    ]
                , Set.fromList
                    [ ("_", "a", Dep)
                    , ("a", "_x0", Dep)
                    , ("_x0", "_1", DepThen)
                    , ("_x0", "_7", DepElse)
                    , ("_1", "b", Dep)
                    , ("b", "_x2", Dep)
                    , ("_x2", "_3", DepThen)
                    , ("_3", "_x4", Dep)
                    , ("_x2", "_5", DepElse)
                    , ("_5", "_x6", Dep)
                    , ("_7", "_x8", Dep)
                    ]
                )
    , testCase "create a single node for the sum of multiple nodes" $
        _createDependencyGraph [] 
            (Op Add 
                (Op Add 
                    (App (Var "foo") (Lit (LInt 1))) 
                    (App (Var "bar") (Lit (LInt 2)))) 
                (App (App (Var "baz") (App (Var "bong") (Lit (LInt 3)))) (Lit (LInt 4))))
            @?= ( Map.fromList 
                    [ ("_", Scope)
                    , ("_x1", Expression (DApp "foo" [DLit (DInt 1)]))
                    , ("_x2", Expression (DApp "bar" [DLit (DInt 2)]))
                    , ("_x3", Expression (DApp "baz" [DVar "_x4", DLit (DInt 4)]))
                    , ("_x4", Expression (DApp "bong" [DLit (DInt 3)]))
                    , ("_x0", Expression (DOp Add (DOp Add (DVar "_x1") (DVar "_x2")) (DVar "_x3")))
                    ]
                , Set.fromList 
                    [ ("_", "_x1", Dep)
                    , ("_", "_x2", Dep)
                    , ("_", "_x4", Dep)
                    , ("_x4", "_x3", Dep)
                    , ("_x1", "_x0", Dep)
                    , ("_x2", "_x0", Dep)
                    , ("_x3", "_x0", Dep)
                    ]
                )
    , testCase "does not create intermediate nodes for let definitions" $
        _createDependencyGraph ["n"]
            (Let [ Def "a" (App (Var "foo") (Var "n"))
                 , Def "b" (App (Var "bar") (Var "n"))
                 ] (Op Add (Var "a") (Var "b")))
            @?= ( Map.fromList
                    [ ("_", Scope)
                    , ("a", Expression (DApp "foo" [DVar "n"]))
                    , ("b", Expression (DApp "bar" [DVar "n"]))
                    , ("_x0", Expression (DOp Add (DVar "a") (DVar "b")))
                    ]
                , Set.fromList
                    [ ("_", "a", Dep)
                    , ("_", "b", Dep)
                    , ("a", "_x0", Dep)
                    , ("b", "_x0", Dep)
                    ]
            )
    , testCase "atomic function calls do not create new nodes" $
        _createDependencyGraph ["n"]
            (Op Add
                (Op Add
                    (App 
                        (App 
                            (Var "schwoop") 
                            (App 
                                (Var "head")
                                (App (App (App (Var "foo") (Lit (LInt 1))) (Lit (LInt 2))) (Lit (LInt 3)))))
                        (App (App (App (Var "scoop") (Lit (LInt 1))) (Lit (LInt 2))) (Lit (LInt 3))))
                    (App (Var "head") (Lit (LList [Lit (LInt 1), Lit (LInt 2), Lit (LInt 3)]))))
                (App (Var "head") (App (Var "bar") (Lit (LInt 1)))))
            @?= ( Map.fromList
                    [ ("_", Scope)
                    , ("_x0", Expression (DOp Add (DOp Add (DVar "_x1") (DAtomApp "head" [DLit (DList [DLit (DInt 1), DLit (DInt 2), DLit (DInt 3)])])) (DAtomApp "head" [DVar "_x4"])))
                    , ("_x1", Expression (DApp "schwoop" [DAtomApp "head" [DVar "_x2"], DVar "_x3"]))
                    , ("_x2", Expression (DApp "foo" [DLit (DInt 1), DLit (DInt 2), DLit (DInt 3)]))
                    , ("_x3", Expression (DApp "scoop" [DLit (DInt 1), DLit (DInt 2), DLit (DInt 3)]))
                    , ("_x4", Expression (DApp "bar" [DLit (DInt 1)]))
                    ]
                , Set.fromList
                    [ ("_", "_x2", Dep)
                    , ("_", "_x3", Dep)
                    , ("_", "_x4", Dep)
                    , ("_x2", "_x1", Dep)
                    , ("_x3", "_x1", Dep)
                    , ("_x4", "_x0", Dep)
                    , ("_x1", "_x0", Dep)
                    ]
                )
    , testCase "function calls embed atomic expressions" $
        _createDependencyGraph ["n"]
            (App (App (App (Var "schwoop") (App (Var "head") (Var "n")))
                (Op Add (App (Var "head") (Var "n")) (Lit (LInt 1))))
                (Op Add (Op Add (App (Var "head") (Var "n")) (Lit (LInt 1))) (Lit (LInt 1)))
            )
        @?= ( Map.fromList
                [ ("_", Scope)
                , ("_x0", Expression (DApp "schwoop" [DAtomApp "head" [DVar "n"], DOp Add (DAtomApp "head" [DVar "n"]) (DLit (DInt 1)), DOp Add (DOp Add (DAtomApp "head" [DVar "n"]) (DLit (DInt 1))) (DLit (DInt 1))]))
                ]
            , Set.fromList [ ("_", "_x0", Dep)]
            )
    ]