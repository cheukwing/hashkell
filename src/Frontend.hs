module Frontend (
    Frontend.Error.Error(..),
    Frontend.Complexity.Cplx(..),
    Frontend.Verification.FunctionData(..),
    Frontend.Verification.FunctionTable,
    pipeline,
) where

import Frontend.Aggregator
import Frontend.Error
import Frontend.Verification
import Frontend.Complexity
import Hashkell.Syntax

import Data.Either (Either)

-- pipeline aggregates information for the same function together
-- and then verifies that the complexity annotation for a function is
-- valid and compatible
pipeline :: Prog -> Either Error FunctionTable
pipeline p = aggregate p >>= verify