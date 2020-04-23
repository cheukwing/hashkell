module Frontend.Verification where

import Hashkell.Syntax
import Frontend.Complexity
import Frontend.Aggregator
import Frontend.Error

import qualified Data.Map.Strict as Map
import Data.Maybe (maybe)
import Control.Monad (when, unless)
import Control.Monad.Except (throwError)

-- verify checks that the aggregations in an aggregation table are all correct
-- with respect to their complexity annotation
-- NOTE: verification such as general type checking is not considered as we
-- assume input programs are correct when annotations are removed, and because
-- the final output program will be verified by GHC/similar compiler anyway
verify :: AggregationTable -> Either Error AggregationTable
verify at 
    = mapM_ verifyAggregation (Map.elems at) >> return at

-- verifyAggregation checks that the complexity annotation in an aggregation
-- is compatible with the other information given for a certain function
verifyAggregation :: Aggregation -> Either Error ()
-- if we just have complexity and types, then if we do have a param in our
-- complexity, ensure that it will have some assigned type
-- NOTE: most likely not necessary, since ts should not be empty if parsed
verifyAggregation (Just c, Just ts, Nothing)
    = maybe (return ()) 
        (\_ -> when (null ts) $ throwError IncompatibleComplexity)
        (paramComplexity c)
-- if we just have complexity and definition, then if we do have a param in our
-- complexity, ensure that it is also present in the params of the definition
verifyAggregation (Just c, Nothing, Just (params, _))
    = maybe (return ())
        (\n -> when (n `notElem` params) $ throwError IncompatibleComplexity)
        (paramComplexity c)
-- if we have all the information and we do have a param in our complexity,
-- then get the type of that param (simultaneously checking that the param
-- exists and has a type), then check that the type is supported for
-- use as a complexity
verifyAggregation (Just c, Just ts, Just (params, _))
    = maybe (return ())
        (maybe (throwError IncompatibleComplexity)
            (\t -> unless (isSupportedType t) 
                    $ throwError IncompatibleComplexity)
            . typeOf)
        (paramComplexity c)
    where
        isSupportedType Bool   = False
        isSupportedType Int    = True
        isSupportedType List{} = True
        typeOf name 
            = case [t | (t, p) <- zip ts params, p == name] of
                (t : _) -> Just t
                []      -> Nothing
-- if we do not have a complexity, or just a complexity, then ignore
verifyAggregation _
    = return ()