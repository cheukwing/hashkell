import Simple.Parser (parseProg)
import Simple.Syntax
import Parallelizer (buildFunctionTable, splitFunction, complexityCondition)

import qualified Data.Map.Strict as Map

import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" 
            [ parserTests
            , buildFunctionTableTests
            , splitFunctionTests
            , complexityConditionTests
            ]


parserTests = testGroup "Parser tests"
    [ testCase "parses simple function" $
        parseProg "a = 1;" @?= 
            Right [Func (FuncData "a" [] (Lit (LInt 1)))]
    , testCase "parses multiple function definitions" $
        parseProg "a = 1;\nb = 2;" @?= 
            Right [ Func (FuncData "a" [] (Lit (LInt 1)))
                  , Func (FuncData "b" [] (Lit (LInt 2)))
                  ]
    , testCase "parses addition" $
        parseProg "a = 1 + 2;" @?= 
            Right [Func (FuncData "a" [] (Op Add (Lit (LInt 1)) (Lit (LInt 2))))]
    , testCase "parses function application" $
        parseProg "a = b c d;" @?= 
            Right [Func (FuncData "a" [] (App (App (Var "b") (Var "c")) (Var "d")))]
    , testCase "ignores comment and parses function" $
        parseProg "-- some comment here\n a = 1;" @?=
            Right [Func (FuncData "a" [] (Lit (LInt 1)))]
    , testCase "parses arguments" $
        parseProg "foobar a b c = a;" @?=
            Right [Func (FuncData "foobar" ["a", "b", "c"] (Var "a"))]
    ]


buildFunctionTableTests = testGroup "buildFunctionTable tests"
    [ testCase "creates entry for just complexity" $
        buildFunctionTable [Complexity "a" [Polynomial 1, Exponential]] @?=
          Map.fromList [("a", [("?", Polynomial 1), ("?", Exponential)])]  
    , testCase "creates entry for just function" $
        buildFunctionTable [Func (FuncData "a" ["b", "c"] (Lit (LInt 1)))] @?=
          Map.fromList [("a", [("b", None), ("c", None)])]  
    , testCase "updates entry for function" $
        buildFunctionTable 
            [ Complexity "a" [Polynomial 1, Exponential]
            , Func (FuncData "a" ["b", "c"] (Lit (LInt 1)))
            ] @?=
          Map.fromList [("a", [("b", Polynomial 1), ("c", Exponential)])]  
    , testCase "updates entry for complexity" $
        buildFunctionTable 
            [ Func (FuncData "a" ["b", "c"] (Lit (LInt 1)))
            , Complexity "a" [Polynomial 1, Exponential]
            ] @?=
          Map.fromList [("a", [("b", Polynomial 1), ("c", Exponential)])]  
    , testCase "does not drop unknown arg complexities" $
        buildFunctionTable 
            [ Func (FuncData "a" ["b", "c", "d"] (Lit (LInt 1)))
            , Complexity "a" [Polynomial 1, Exponential]
            ] @?=
          Map.fromList [("a", [("b", Polynomial 1), ("c", Exponential), ("d", None)])]  
    , testCase "does drop extra complexities not corresponding to an arg" $
        buildFunctionTable 
            [ Complexity "a" [Polynomial 1, Exponential, Polynomial 2]
            , Func (FuncData "a" ["b", "c"] (Lit (LInt 1)))
            ] @?=
          Map.fromList [("a", [("b", Polynomial 1), ("c", Exponential)])]  
    ]


splitFunctionTests = testGroup "splitFunction tests"
    []


complexityConditionTests = testGroup "complexityCondition tests"
    [ testCase "false condition for none" $
        complexityCondition 100 ("b", None) @?= Lit (LBool False)    
    , testCase "creates condition for exponential time" $
        complexityCondition 100 ("b", Exponential) @?= Op GTE (Var "b") (Lit (LInt 7))
    , testCase "creates condition for linear time" $
        complexityCondition 100 ("b", Polynomial 1) @?= Op GTE (Var "b") (Lit (LInt 100))
    , testCase "creates condition for quadratic time" $
        complexityCondition 100 ("b", Polynomial 2) @?= Op GTE (Var "b") (Lit (LInt 10))
    , testCase "creates condition for cubic time" $
        complexityCondition 100 ("b", Polynomial 3) @?= Op GTE (Var "b") (Lit (LInt 5))
    ]