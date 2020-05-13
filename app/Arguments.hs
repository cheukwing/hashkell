module Arguments (
    parseArgs,
    fileName,
    toContext
) where

import Context

import Options.Applicative
import Data.Semigroup ((<>))
import qualified Data.Maybe as Maybe
import System.FilePath.Posix as Posix

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

data ParType
    = ParSequential
    | ParAll
    | ParFunction
    | ParPathed
    deriving Show

data ParalleliseOpts = ParallelOpts
    { parType :: ParType
    , parOut :: Maybe String
    }
    deriving Show

fileName :: Args -> String
fileName (Parallelise _ copts)
    = optFile copts
fileName (Graph _ copts)
    = optFile copts

toContext :: Args -> Context
toContext (Parallelise popts copts)
    = Context
        { ctxSteps = optSteps copts
        , ctxAtomic = optAtomic copts
        , ctxRedundant = optRedundant copts
        , ctxParType = fromParType (parType popts)
        , ctxDrawAll = False
        , ctxOutput = Maybe.fromMaybe ("./out/par_" ++ Posix.takeFileName (optFile copts)) (parOut popts)
        }
    where
        fromParType ParSequential = Sequentially
        fromParType ParAll = AllParallel
        fromParType ParFunction = FunctionOnly
        fromParType ParPathed = Pathed
toContext (Graph gopts copts)
    = Context
        { ctxSteps = optSteps copts
        , ctxAtomic = optAtomic copts
        , ctxRedundant = optRedundant copts
        , ctxParType = None
        , ctxDrawAll = graphAll gopts
        , ctxOutput = ""
        }

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
parseParalleliseOpts
    = ParallelOpts
        <$> subparser 
            ( command "all"
                (info (pure ParAll) (progDesc "Encode parallelisable functions with maximum parallelisation."))
           <> command "seq"
                (info (pure ParSequential) (progDesc "Encode parallelisable functions with no parallelisation (for testing correctness)."))
           <> command "function"
                (info (pure ParFunction) (progDesc "Encode parallelisable functions with only function calls parallelised."))
           <> command "pathed"
                (info (pure ParPathed) (progDesc "Encode parallelisable functions with only function calls parallelised, and leaving one path sequential for the main thread.")))
        <*> optional 
            ( strOption 
                ( long "output-file" 
               <> short 'o'
               <> metavar "FILENAME"
               <> help "Output file name"
               ))
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