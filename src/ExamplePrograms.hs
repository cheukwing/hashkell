module ExamplePrograms where

import Simple.Syntax
import DependencyGraph

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

import qualified Data.GraphViz as G
import qualified Data.GraphViz.Attributes.Complete as G
import qualified Data.GraphViz.Types as G

naiveFibProg :: Prog
naiveFibProg = [Cplx "fib" (Op Exp (Lit (LInt 2)) (Var "n")), naiveFibFunc]

naiveFibFunc :: Decl
naiveFibFunc =
    Func "fib" ["n"] 
        (If (Op Simple.Syntax.EQ (Var "n") (Lit (LInt 0))) 
            (Lit (LInt 1)) 
            (If (Op Simple.Syntax.EQ (Var "n") (Lit (LInt 1))) 
                (Lit (LInt 1)) 
                (Op Add 
                    (App (Var "fib") (Op Sub (Var "n") (Lit (LInt 1)))) 
                    (App (Var "fib") (Op Sub (Var "n") (Lit (LInt 2))))
                )
            )
        )

drawDependencyGraph :: Decl -> IO ()
drawDependencyGraph (Func _ args expr) = do
    let 
        (ns, ds) = toDependencyGraph args expr
        dotGraph = G.graphElemsToDot depGraphParams ns ds
        dotText = G.printDotGraph dotGraph :: TL.Text
    TL.writeFile "files.dot" dotText