module Main where

import Frontend
import Middleend
import Backend
import Hashkell.Parser (parseProg)
import Hashkell.Syntax (Prog)
import Arguments
import Context

import Data.Either as Either


main :: IO ()
main = do
    args <- parseArgs
    let ctx = toContext args
    parsed <- parseProg <$> readFile (fileName args)
    case parsed of
        Left err -> putStrLn $ "Parse error: " ++ err
        Right prog -> 
            case process ctx prog of
                Left err -> putStrLn $ "Encountered an error: " ++ show err
                Right io -> io >> putStrLn "Done."

process :: Context -> Prog -> Either Error (IO ())
process ctx prog = do
    ft <- Frontend.pipeline prog
    let eit = Middleend.pipeline ctx ft
    return (Backend.pipeline ctx eit)