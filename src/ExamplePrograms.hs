module ExamplePrograms where

import Hashkell.Syntax

import Prelude hiding (EQ, LT, GT)

naiveFibProg :: Prog
naiveFibProg = 
    [ Cplx "fib" (Op Exp (Lit (LInt 2)) (Var "x"))
    , Type "fib" [Int, Int]
    , naiveFibFunc
    ]

naiveFibFunc :: Decl
naiveFibFunc =
    Func "fib" ["x"] naiveFibExpr

naiveFibExpr :: Expr
naiveFibExpr =
    If (Op LT (Var "x") (Lit (LInt 1)))
        (Lit (LInt 0)) 
        (If (Op LT (Var "x") (Lit (LInt 2))) 
            (Lit (LInt 1))
            (Op Add 
                (App (Var "fib") (Op Sub (Var "x") (Lit (LInt 1)))) 
                (App (Var "fib") (Op Sub (Var "x") (Lit (LInt 2))))))
