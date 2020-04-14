module DependencyGraphTests where
{-

import Simple.Syntax
import DependencyGraph.Internal 
    ( createDependencyGraph
    , encodeDependencyGraph
    , DNode(..)
    , DType(..)
    , DExpr(..)
    )

import Prelude hiding (GT, LT, EQ)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Test.Tasty
import Test.Tasty.HUnit

naiveFibGraph = 
    ( Map.fromList 
        [ ("_", Scope)
        , ("_1", Scope)
        , ("_3", Scope)
        , ("_5", Scope)
        , ("_7", Scope)
        , ("_x0", Expression (DOp EQ (DVar "n") (DLit (LInt 0))))
        , ("_x10", Expression (DOp Sub (DVar "n") (DLit (LInt 2))))
        , ("_x11", Expression (DApp "fib" [DVar "_x10"]))
        , ("_x12", Expression (DOp Add (DVar "_x9") (DVar "_x11")))
        , ("_x13", Conditional (DVar "_x4"))
        , ("_x14", Conditional (DVar "_x0"))
        , ("_x2", Expression (DLit (LInt 1)))
        , ("_x4", Expression (DOp EQ (DVar "n") (DLit (LInt 1))))
        , ("_x6", Expression (DLit (LInt 1)))
        , ("_x8", Expression (DOp Sub (DVar "n") (DLit (LInt 1))))
        , ("_x9", Expression (DApp "fib" [DVar "_x8"]))
        ]
    , Set.fromList 
        [ ("_", "_x0", DepD)
        , ("_", "_x0", DepArg)
        , ("_", "_x10", DepArg)
        , ("_", "_x4", DepArg)
        , ("_", "_x8", DepArg)
        , ("_1", "_x2", DepD)
        , ("_3", "_x4", DepD)
        , ("_5", "_x6", DepD)
        , ("_7", "_x10", DepD)
        , ("_7", "_x8", DepD)
        , ("_x0", "_x14", DepD)
        , ("_x10", "_x11", DepD)
        , ("_x11", "_x12", DepD)
        , ("_x13", "_5", DepThen)
        , ("_x13", "_7", DepElse)
        , ("_x14", "_1", DepThen)
        , ("_x14", "_3", DepElse)
        , ("_x4", "_x13", DepD)
        , ("_x8", "_x9", DepD)
        , ("_x9", "_x12", DepD)
        ]
    )

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
                    , ("b", "_x0", DepD)
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

encodeDependencyGraphTests = testGroup "encodeDependencyGraph tests"
    [ testCase "encode naive fib sequentially" $
        encodeDependencyGraph naiveFibGraph False
            @?= "let _x0 = (n == 0); _x14 = if _x0 then let _x2 = 1 in _x2 else let _x4 = (n == 1); _x13 = if _x4 then let _x6 = 1 in _x6 else let _x10 = (n - 2); _x11 = (fib _x10); _x8 = (n - 1); _x9 = (fib _x8); _x12 = (_x9 + _x11) in _x12;  in _x13;  in _x14" 
    , testCase "encode naive fib parallelised" $
        encodeDependencyGraph naiveFibGraph True 
            @?= "let _x0 = (n == 0); _x14 = if _x0 then let _x2 = 1 in _x2 else let _x4 = (n == 1); _x13 = if _x4 then let _x6 = 1 in _x6 else runEval $ do { let { _x10 = (n - 2) }; _x11 <- rpar (fib _x10); let { _x8 = (n - 1) }; _x9 <- rpar (fib _x8); let { _x12 = (_x9 + _x11) }; return _x12};  in _x13;  in _x14"

    ]
-}