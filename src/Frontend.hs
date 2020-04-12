module Frontend (
    Frontend.Error.Error(..),
    Frontend.Complexity.Cplx(..),
    Frontend.Aggregator.Aggregation(..),
    Frontend.Aggregator.AggregationTable,
    pipeline,
) where

import Frontend.Aggregator
import Frontend.Error
import Frontend.Verification
import Frontend.Complexity
import Simple.Syntax

import Data.Either (Either)

pipeline :: Prog -> Either Error AggregationTable
pipeline p = aggregate p >>= verify