--module Arguments (Arguments(..), runWithArgs, argumentsToContext) where
module Arguments where

import Context

import Options.Applicative
import Data.Semigroup ((<>))

{-
data Arguments = Arguments
    { files          :: [String]
    , parallelise    :: Bool
    , steps          :: Int
    , separateAtomic :: Bool
    , keepRedundant  :: Bool
    , graph          :: Bool
    , drawAll        :: Bool
    }
-}

data Args
    = Parallelise ParalleliseOpts CommonOpts
    | Graph GraphOpts CommonOpts

data CommonOpts = CommonOpts 
    { optFiles :: [String]
    , optSteps :: Int
    , optAtomic :: Bool
    , optRedundant :: Bool
    }

newtype GraphOpts = GraphOpts
    { graphAll :: Bool
    }

data ParalleliseOpts
    = ParSequential
    | ParAll
    | ParDefault

parseCommonOpts :: Parser CommonOpts
parseCommonOpts
    = CommonOpts
        <$> some (argument str 
            ( metavar "FILENAMES..." 
           <> help "The programs to parallelise"))
        <*> option auto
            ( long "steps"
           <> short 's'
           <> help "The number of steps to set the parallelisation boundary to"
           <> value 1000 
            )
        <*> switch
            ( long "separate-atomic"
           <> short 'a'
           <> help
                ("Whether to separate atomic expressions into separate nodes when building the dependency graph. "
                ++ "This is likely to create a larger graph, exposing more parallelism than can actually be exploited.")
            )
        <*> switch
            ( long "keep-redundant"
           <> short 'r'
           <> help
                ("Whether to keep redundant arcs when building the dependency graph. "
                ++ "This is likely to create a more cluttered graph, but should not affect the amount of parallelism actually exposed.")
            )

parseParallelise :: Parser Args
parseParallelise = Parallelise <$> parseParalleliseOpts <*> parseCommonOpts

parseParalleliseOpts :: Parser ParalleliseOpts
parseParalleliseOpts = --pure ParDefault <**> helper
    subparser (
       command "all"
         (info (pure ParAll) (progDesc "1"))
      <> command "seq"
         (info (pure ParSequential) (progDesc "2"))
      <> command "default"
         (info (pure ParDefault) (progDesc "3")))
    <**> helper

parseGraph :: Parser Args
parseGraph = Graph <$> parseGraphOpts <*> parseCommonOpts

parseGraphOpts :: Parser GraphOpts
parseGraphOpts 
    = GraphOpts
       <$> switch
            ( long "draw-all"
           <> short 'A'
           <> help "1"
            )
    <**> helper

arguments :: Parser Args
arguments = subparser $ 
    command "parallelise"
        (info parseParallelise (progDesc "Parallelise the input files"))
    <> command "graph"
        (info parseGraph (progDesc "Draw the dependency graphs for the input files"))

parseArgs :: IO Args
parseArgs = execParser args
    where
        args = info (arguments <**> helper)
                (fullDesc <> progDesc "1" <> header "2")



{-
runWithArgs :: (Arguments -> IO()) -> IO ()
runWithArgs f
    = execParser args >>= f
    where args = info (arguments <**> helper)
            ( fullDesc
            <> progDesc "Adds parallelisation strategies to Hashkell code"
            <> header "Hashkell - Haskell with semi-automatic parallelisation"
            )

arguments :: Parser Arguments
arguments 
    = Arguments
        <$> some (argument str 
            ( metavar "FILENAMES..." 
           <> help "The programs to parallelise"))
        <*> switch
            ( long "parallelise"
           <> short 'p'
           <> help "Whether to parallelise the input programs"
            )
        <*> option auto
            ( long "steps"
           <> short 's'
           <> help "The number of steps to set the parallelisation boundary to"
           <> value 1000 
            )
        <*> switch
            ( long "separate-atomic"
           <> short 'a'
           <> help
                ("Whether to separate atomic expressions into separate nodes when building the dependency graph. "
                ++ "This is likely to create a larger graph, exposing more parallelism than can actually be exploited.")
            )
        <*> switch
            ( long "keep-redundant"
           <> short 'r'
           <> help
                ("Whether to keep redundant arcs when building the dependency graph. "
                ++ "This is likely to create a more cluttered graph, but should not affect the amount of parallelism actually exposed.")
            )
        <*> switch
            ( long "graph"
           <> short 'g'
           <> help "Whether to draw the graph of parallelisable functions"
            )
        <*> switch
            ( long "draw-all"
           <> short 'A'
           <> help "Whether to draw the graphs of all functions"
            )

argumentsToContext :: Arguments -> Context
argumentsToContext args
    = Context 
        { boundarySteps    = steps args 
        , fewerAtomicNodes = not (separateAtomic args)
        , noRedundantArcs  = not (keepRedundant args)
        }
-}