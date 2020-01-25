import Simple.Parser (parseProg)
import Simple.Syntax

import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ parserTests ]

parserTests = testGroup "Parser tests"
    [ testCase "parses simple function" $
        parseProg "a = 1;" @?= Right [Func "a" [] (Lit (LInt 1))]
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
