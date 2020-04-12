module Frontend.Complexity where

import Simple.Syntax
import Frontend.Error

import Data.Either (Either)
import Control.Monad.Except (throwError)

-- TODO: support factorial
data Cplx
    = Constant Int
    | Polynomial Name Int
    | Exponential Int Name
    | Logarithmic Name
    deriving (Eq, Show)


parseComplexity :: Expr -> Either Error Cplx
parseComplexity If{}
    = throwError IllegalComplexity
parseComplexity Let{} 
    = throwError IllegalComplexity
parseComplexity (App (Var "log") (Var name)) 
    = return (Logarithmic name)
parseComplexity (App (Var "log") _) 
    = throwError UnsupportedComplexity
parseComplexity App{}
    = throwError IllegalComplexity
parseComplexity (Var name)
    = return (Polynomial name 1)
parseComplexity (Lit (LInt n))
    = return (Constant n)
parseComplexity Lit{}
    = throwError IllegalComplexity
parseComplexity (Op Exp (Var name) (Lit (LInt n)))
    = return (Polynomial name n)
parseComplexity (Op Exp (Lit (LInt n)) (Var name))
    = return (Exponential n name)
parseComplexity Op{}
    = throwError UnsupportedComplexity

paramComplexity :: Cplx -> Maybe String
paramComplexity Constant{}        = Nothing
paramComplexity (Polynomial n _)  = Just n
paramComplexity (Exponential _ n) = Just n
paramComplexity (Logarithmic n)   = Just n