module Frontend.Verification where

import Simple.Syntax
import Frontend.Complexity
import Frontend.Aggregator
import Frontend.Error

import qualified Data.Map.Strict as Map
import Data.Maybe (maybe)
import Control.Monad (when, unless)
import Control.Monad.Except (throwError)


verify :: AggregationTable -> Either Error AggregationTable
verify at 
    = mapM_ verifyAggregation (Map.elems at) >> return at


verifyAggregation :: Aggregation -> Either Error ()
verifyAggregation Complexity{}
    = return ()
verifyAggregation TypeAnnotation{}
    = return ()
verifyAggregation Definition{}
    = return ()
verifyAggregation (ComplexityType c ts)
    = maybe (return ()) 
        (\_ -> when (null ts) $ throwError IncompatibleComplexity)
        (paramComplexity c)
verifyAggregation (ComplexityDefinition c params _)
    = maybe (return ())
        (\n -> when (n `notElem` params) $ throwError IncompatibleComplexity)
        (paramComplexity c)
verifyAggregation TypeDefinition{}
    = return ()
verifyAggregation (Complete c ts params _)
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