module Context where

type Steps = Int
data Context = Context
    { ctxSteps :: Steps
    , ctxAtomic :: Bool
    , ctxRedundant :: Bool
    , ctxParType :: ParallelisationStrategy
    , ctxDrawAll :: Bool
    , ctxOutput :: String
    }

data ParallelisationStrategy
    = Sequentially
    | AllParallel
    | FunctionOnly
    | Pathed
    | None
    deriving Eq