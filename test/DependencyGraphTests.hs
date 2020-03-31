module DependencyGraphTests (dependencyGraphTests) where

import Simple.Syntax
import DependencyGraph.Internal 
    ( createDependencyGraph
    , DNode(..)
    , DType(..)
    , DExpr(..)
    )

import Prelude hiding (GT, LT, EQ)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Test.Tasty
import Test.Tasty.HUnit


dependencyGraphTests :: TestTree
dependencyGraphTests = testGroup "DependencyGraph tests" 
            [ createDependencyGraphTests
            ]

createDependencyGraphTests = testGroup "createDependencyGraph tests"
    [ testCase "create very basic graph" $
        createDependencyGraph [] (Lit (LInt 1))
            @?= ( Map.fromList 
                    [ ("_", Scope)
                    , ("_x0", Expression (DLit (LInt 1)))
                    ]
                , Set.fromList [("_", "_x0", DepD)]
                )
    , testCase "create very basic graph with argument" $
        createDependencyGraph ["n"] (Var "n")
            @?= ( Map.fromList 
                    [ ("_", Scope)
                    , ("_x0", Expression (DVar "n"))
                    ]
                , Set.fromList 
                    [ ("_", "_x0", DepD)
                    , ("_", "_x0", DepArg)
                    ]
                )
    , testCase "create atomic binary operation graph" $
        createDependencyGraph ["n"] (Op Add (Op Add (Lit (LInt 1)) (Lit (LInt 2))) (Lit (LInt 3)))
            @?= ( Map.fromList 
                    [ ("_", Scope)
                    , ("_x0", Expression (DOp Add (DOp Add (DLit (LInt 1)) (DLit (LInt 2))) (DLit (LInt 3))))
                    ]
                , Set.fromList [("_", "_x0", DepD)]
                )
    , testCase "create binary operation graph" $
        createDependencyGraph ["n"] (Op Add (Var "n") (Lit (LInt 3)))
            @?= ( Map.fromList 
                    [ ("_", Scope)
                    , ("_x0", Expression (DOp Add (DVar "n") (DLit (LInt 3))))
                    ]
                , Set.fromList [("_", "_x0", DepD), ("_", "_x0", DepArg)]
                )
    , testCase "create function application graph" $
        createDependencyGraph ["n"] (App (App (Var "someFunc") (Var "n")) (Lit (LInt 1)))
            @?= ( Map.fromList 
                    [ ("_", Scope)
                    , ("_x0", Expression (DApp "someFunc" [DVar "n", DLit (LInt 1)]))
                    ]
                , Set.fromList [("_", "_x0", DepD), ("_", "_x0", DepArg)]
                )
    , testCase "create let graph" $
        createDependencyGraph ["n"] 
            (Let [ Def "a" (Lit (LInt 1))
                 , Def "b" (Lit (LInt 2))
                 ] 
                 (Op Add (Var "n") (Var "a"))
            )
            @?= ( Map.fromList 
                    [ ("_", Scope)
                    , ("a", Expression (DLit (LInt 1)))
                    , ("b", Expression (DLit (LInt 2)))
                    , ("_x0", Expression (DOp Add (DVar "n") (DVar "a")))
                    ]
                , Set.fromList 
                    [ ("_", "_x0", DepD)
                    , ("_", "_x0", DepArg)
                    , ("a", "_x0", DepD)
                    , ("_", "a", DepD)
                    , ("_", "b", DepD)
                    ]
                )
    , testCase "create if graph" $
        createDependencyGraph ["a", "b"] 
            (If (Op LT (Var "a") (Lit (LInt 1)))
                (Lit (LInt 2))
                (App (Var "foobar") (Op Add (Var "a") (Var "b")))
            )
            @?= ( Map.fromList 
                    [ ("_", Scope)
                    , ("_x0", Expression (DOp LT (DVar "a") (DLit (LInt 1))))
                    , ("_1", Scope)
                    , ("_x2", Expression (DLit (LInt 2)))
                    , ("_3", Scope)
                    , ("_x4", Expression (DOp Add (DVar "a") (DVar "b")))
                    , ("_x5", Expression (DApp "foobar" [DVar "_x4"]))
                    , ("_x6", Conditional (DVar "_x0"))
                    ]
                , Set.fromList 
                    [ ("_", "_x0", DepD)
                    , ("_", "_x0", DepArg)
                    , ("_x0", "_x6", DepD)
                    , ("_x6", "_1", DepThen)
                    , ("_x6", "_3", DepElse)
                    , ("_1", "_x2", DepD)
                    , ("_3", "_x4", DepD)
                    , ("_", "_x4", DepArg)
                    , ("_x4", "_x5", DepD)
                    ]
                )
            
    ]