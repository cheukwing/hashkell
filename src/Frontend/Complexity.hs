module Frontend.Complexity where

import Hashkell.Syntax
import Frontend.Error

import Data.Either (Either)
import Control.Monad.Except (throwError)

data Cplx
    = Constant Int
    | Polynomial Name Int
    | Exponential Int Name
    | Logarithmic Name
    | Factorial Name
    deriving (Eq, Show)


-- parseComplexity returns the Cplx for a valid and supported complexity
-- annotation, or an error otherwise
--
-- Illegal:
-- - if ... then .. else
-- - let ... in ...
-- - someFunc ...
-- - True / False / [1, 2, ...] / etc
-- 
-- Unsupported (but parseable):
-- - log 10 / logBase ... / fac 10 / ...
-- - 1 + 1 / 10 * x / etc
parseComplexity :: Expr -> Either Error Cplx
parseComplexity If{}
    = throwError IllegalComplexity
parseComplexity Let{} 
    = throwError IllegalComplexity
parseComplexity (App (Var "log") (Var name)) 
    = return (Logarithmic name)
parseComplexity (App (Var "log") _) 
    = throwError UnsupportedComplexity
parseComplexity (App (Var "fac") (Var name)) 
    = return (Factorial name)
parseComplexity (App (Var "fac") _) 
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

-- paramComplexity retrieves the name of the parameter referred to in the
-- complexity if it exists, or Nothing otherwise
paramComplexity :: Cplx -> Maybe String
paramComplexity Constant{}        = Nothing
paramComplexity (Polynomial n _)  = Just n
paramComplexity (Exponential _ n) = Just n
paramComplexity (Logarithmic n)   = Just n
paramComplexity (Factorial n)   = Just n