module Arguments (
    parseArgs,
    Args(..),
    CommonOpts(..),
    GraphOpts(..),
    ParalleliseOpts(..)
) where

import Context

import Options.Applicative
import Data.Semigroup ((<>))

data Args
    = Parallelise ParalleliseOpts CommonOpts
    | Graph GraphOpts CommonOpts
    deriving Show

data CommonOpts = CommonOpts 
    { optFile :: String
    , optSteps :: Int
    , optAtomic :: Bool
    , optRedundant :: Bool
    }
    deriving Show

newtype GraphOpts = GraphOpts
    { graphAll :: Bool
    }
    deriving Show

data ParalleliseOpts
    = ParSequential
    | ParAll
    | ParFunction
    | ParPathed
    deriving Show

parseCommonOpts :: Parser CommonOpts
parseCommonOpts
    = CommonOpts
        <$> argument str 
            ( metavar "FILENAME" 
           <> help "The program to parallelise")
        <*> option auto
            ( long "steps"
           <> short 's'
           <> metavar "NUM"
           <> help "The number of steps to approximate parallelisation overhead"
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
parseParalleliseOpts =
    subparser (
       command "all"
         (info (pure ParAll) (progDesc "Encode parallelisable functions with maximum parallelisation."))
      <> command "seq"
         (info (pure ParSequential) (progDesc "Encode parallelisable functions with no parallelisation (for testing correctness)."))
      <> command "function"
         (info (pure ParFunction) (progDesc "Encode parallelisable functions with only function calls parallelised."))
      <> command "pathed"
         (info (pure ParPathed) (progDesc "Encode parallelisable functions with only function calls parallelised, and leaving one path sequential for the main thread.")))
    <**> helper

parseGraph :: Parser Args
parseGraph = Graph <$> parseGraphOpts <*> parseCommonOpts

parseGraphOpts :: Parser GraphOpts
parseGraphOpts 
    = GraphOpts
       <$> switch
            ( long "draw-all"
           <> short 'A'
           <> help "Draw all functions, even those not deemed parallelisable"
            )
    <**> helper

arguments :: Parser Args
arguments = subparser $ 
    command "parallelise"
        (info parseParallelise (progDesc "Parallelise the input file"))
    <> command "graph"
        (info parseGraph (progDesc "Draw the dependency graphs for the input file"))

parseArgs :: IO Args
parseArgs = execParser args
    where
        args = info (arguments <**> helper)
                ( fullDesc 
               <> progDesc "Compiles sequential Hashkell code into parallelised Haskell code"
               <> header "Hashkell to Parallelised Haskell Compiler"
                )