import Simple.Parser (parseProg)
import Simple.Syntax as Syntax

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Test.Tasty
import Test.Tasty.HUnit

import ParallelizerTests
import DependencyGraphTests

main = defaultMain $ testGroup "Tests" 
        [ parserTests
        , parallelizerTests
        , dependencyGraphTests
        ]

parserTests :: TestTree
parserTests = testGroup "Parser Tests" 
            [ parseProgTests
            ]


parseProgTests = testGroup "parseProg tests"
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
    , testCase "parses complexity" $
        parseProg "foobar ## n;" @?=
            Right [Cplx "foobar" (Var "n")]
    , testCase "parses regular types" $
        parseProg "foobar :: Int -> Int;" @?=
            Right [Type "foobar" [Int, Int]]
    , testCase "parses cons" $
        parseProg "foobar = 1 : 2 : []" @?=
            Right [Func "foobar" [] (Op Cons (Lit (LInt 1)) (Op Cons (Lit (LInt 2)) (Lit (LList []))))]
    , testCase "parses list types" $
        parseProg "foobar :: [Int] -> [[Bool]] -> Int -> Bool" @?=
            Right [Type "foobar" [List Int, List (List Bool), Int, Bool]]
    ]


{-
buildFunctionTableTests = testGroup "buildFunctionTable tests" 
    [ testCase "creates entry for just function" $
        buildFunctionTable [Func "bunglebionics" ["n"] basicFunctionDefn]
            @?= Map.fromList [("bunglebionics", (["n"], basicFunctionDefn, Lit (LInt 1)))]
    , -- TODO: may want to reconsider this behaviour
      testCase "creates entry for just annotation" $
        buildFunctionTable [Cplx "bunglebionics" (Var "n")]
            @?= Map.fromList [("bunglebionics", ([], Lit (LInt 1), Var "n"))]
    , testCase "updates function entry when annotation reached" $
        buildFunctionTable [Func "bunglebionics" ["n"] basicFunctionDefn, Cplx "bunglebionics" (Var "n")]
            @?= Map.fromList [("bunglebionics", (["n"], basicFunctionDefn, Var "n"))]
    , testCase "updates annotation entry when function reached" $
        buildFunctionTable [Cplx "bunglebionics" (Var "n"), Func "bunglebionics" ["n"] basicFunctionDefn]
            @?= Map.fromList [("bunglebionics", (["n"], basicFunctionDefn, Var "n"))]
    , testCase "keeps function entry when annotation does not match" $
        buildFunctionTable [Func "bunglebionics" ["n"] basicFunctionDefn, Cplx "bunglebionics" (Var "c")]
            @?= Map.fromList [("bunglebionics", (["n"], basicFunctionDefn, Lit (LInt 1)))]
    , testCase "discards annotation entry when function does not match" $
        buildFunctionTable [Cplx "bunglebionics" (Var "c"), Func "bunglebionics" ["n"] basicFunctionDefn]
            @?= Map.fromList [("bunglebionics", (["n"], basicFunctionDefn, Lit (LInt 1)))]
    , testCase "can construct from multiple functions" $
        buildFunctionTable [ Cplx "bunglebionics" (Var "n")
                           , Func "bunglebionics" ["n"] basicFunctionDefn
                           , Func "bungletronics" ["n", "m", "o"] basicFunctionDefn
                           , Cplx "bungletronics" (Op Mul (Var "n") (Var "m"))
                           ]
            @?= Map.fromList [ ("bunglebionics", (["n"], basicFunctionDefn, Var "n"))
                             , ("bungletronics", (["n", "m", "o"], basicFunctionDefn, Op Mul (Var "n") (Var "m")))
                             ]
    ]

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
-}
