import Simple.Parser (parseProg)
import Simple.Syntax

import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ parserTests ]

parserTests = testGroup "Parser tests"
    [ testCase "function definition literal" $
        parseProg "a = 1;" @?= Right [Func "a" [] (Lit (LInt 1))]
    , testCase "multiple function definition" $
        parseProg "a = 1;\nb = 2;" @?= 
            Right [ Func "a" [] (Lit (LInt 1))
                  , Func "b" [] (Lit (LInt 2))
                  ]
    , testCase "function definition addition" $
        parseProg "a = 1 + 2;" @?= 
            Right [Func "a" [] (Op Add (Lit (LInt 1)) (Lit (LInt 2)))]
    , testCase "function definition application" $
        parseProg "a = b c d;" @?= 
            Right [Func "a" [] (App (App (Var "b") (Var "c")) (Var "d"))]
    ]
