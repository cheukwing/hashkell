import Simple.Parser (parseProg)
import Simple.Syntax
import Parallelizer (buildFunctionTable, isValidComplexity, freeVariables)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" 
            [ parserTests
            , buildFunctionTableTests
            , isValidComplexityTests
            , freeVariablesTests
            ]


parserTests = testGroup "Parser tests"
    [ testCase "parses simple function" $
        parseProg "a = 1;" @?= 
            Right [Func  "a" [] (Lit (LInt 1))]
    , testCase "parses multiple function definitions" $
        parseProg "a = 1;\nb = 2;" @?= 
            Right [ Func "a" [] (Lit (LInt 1))
                  , Func "b" [] (Lit (LInt 2))
                  ]
    , testCase "parses addition" $
        parseProg "a = 1 + 2;" @?= 
            Right [Func "a" [] (Op Add (Lit (LInt 1)) (Lit (LInt 2)))]
    , testCase "parses function application" $
        parseProg "a = b c d;" @?= 
            Right [Func "a" [] (App (App (Var "b") (Var "c")) (Var "d"))]
    , testCase "ignores comment and parses function" $
        parseProg "-- some comment here\n a = 1;" @?=
            Right [Func "a" [] (Lit (LInt 1))]
    , testCase "parses arguments" $
        parseProg "foobar a b c = a;" @?=
            Right [Func "foobar" ["a", "b", "c"] (Var "a")]
    ]


buildFunctionTableTests = testGroup "buildFunctionTable tests" []

isValidComplexityTests = testGroup "isValidComplexity tests" 
    [ testCase "validates constant time" $
        isValidComplexity (Lit (LInt 5)) @?= True
    , testCase "validates linear time" $
        isValidComplexity (Var "n") @?= True
    , testCase "validates quadratic time" $
        isValidComplexity (Op Exp (Var "n") (Lit (LInt 2))) @?= True
    , testCase "validates exponential time" $
        isValidComplexity (Op Exp (Lit (LInt 2)) (Var "n")) @?= True
    , testCase "validates log time" $
        isValidComplexity (App (Var "log") (Var "n")) @?= True
    , testCase "validates multiple vars" $
        isValidComplexity (Op Mul (Op Exp (Var "n") (Lit (LInt 3))) (Op Exp (Var "m") (Lit (LInt 3)))) @?= True
    , testCase "invalidates use of bool" $
        isValidComplexity (Lit (LBool True)) @?= False
    , testCase "invalidates non-numerical operations" $
        isValidComplexity (Op GTE (Lit (LInt 1)) (Lit (LInt 1))) @?= False
    , testCase "invalidates if statements" $
        isValidComplexity (Op Add (Lit (LInt 1)) (If (Lit (LBool True)) (Lit (LInt 2)) (Lit (LInt 3)))) @?= False
    , testCase "invalidates non-log applications" $
        isValidComplexity (App (Var "bungletron") (Var "n")) @?= False
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