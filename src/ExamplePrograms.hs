module ExamplePrograms where

import Simple.Syntax


naiveFibProg :: Prog
naiveFibProg = [Cplx "fib" (Op Exp (Lit (LInt 2)) (Var "n")), Type "fib" [Int, Int], naiveFibFunc]


naiveFibDefProg :: Prog
naiveFibDefProg = [Cplx "fib" (Op Exp (Lit (LInt 2)) (Var "n")), Type "fib" [Int, Int], naiveFibDefFunc]


naiveFibFunc :: Decl
naiveFibFunc =
    Func "fib" ["n"] naiveFibExpr
        

naiveFibDefFunc :: Decl
naiveFibDefFunc =
    Func "fib" ["n"]  naiveFibDefExpr


naiveFibExpr :: Expr
naiveFibExpr =
        If (Op Simple.Syntax.EQ (Var "n") (Lit (LInt 0))) 
           (Lit (LInt 1)) 
           (If (Op Simple.Syntax.EQ (Var "n") (Lit (LInt 1))) 
               (Lit (LInt 1)) 
               (Op Add 
                   (App (Var "fib") (Op Sub (Var "n") (Lit (LInt 1)))) 
                   (App (Var "fib") (Op Sub (Var "n") (Lit (LInt 2))))
               )
           )


naiveFibDefExpr :: Expr
naiveFibDefExpr =
        If (Op Simple.Syntax.EQ (Var "n") (Lit (LInt 0))) 
           (Lit (LInt 1)) 
           (If (Op Simple.Syntax.EQ (Var "n") (Lit (LInt 1))) 
               (Lit (LInt 1)) 
               (Let [ Def "a" (App (Var "fib") (Op Sub (Var "n") (Lit (LInt 1))))
                    , Def "b" (App (Var "fib") (Op Sub (Var "n") (Lit (LInt 2)))) 
                    ]
                    (Op Add (Var "a") (Var "b"))
               )
           )
        