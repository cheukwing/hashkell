module Context where

type Steps = Int
data Context = Context
    { boundarySteps    :: Steps
    , fewerAtomicNodes :: Bool
    , noRedundantArcs  :: Bool
    }
