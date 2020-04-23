import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Hashkell.Parser (parseProg)
import Hashkell.Syntax as Syntax

import FrontendTests
import MiddleendTests

main = defaultMain $ testGroup "Tests" 
        [ parserTests
        , frontendTests
        , middleendTests
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