{
-- Adapted from Write You A Haskell
{-# LANGUAGE FlexibleContexts #-}

module Simple.Lexer (
    Token(..),
    scanTokens,
) where

import Simple.Syntax

import Control.Monad.Except
}

%wrapper "basic"

$digit = 0-9
$lower = [a-z]
$alpha = [a-zA-Z]
$eol   = [\n]

tokens :-
    $eol                          ;
    $white+                       ;

    "--".*                        ;

    if                            { \s -> TokenIf }
    then                          { \s -> TokenThen }
    else                          { \s -> TokenElse }
    $digit+                       { \s -> TokenNum (read s) }
    $lower [$alpha $digit \_ \']* { \s -> TokenSym s }
    "=="                          { \s -> TokenEq }
    [\+]                          { \s -> TokenAdd }
    [\-]                          { \s -> TokenSub }
    \(                            { \s -> TokenLParen }
    \)                            { \s -> TokenRParen }

{
data Token
    = TokenIf
    | TokenThen
    | TokenElse
    | TokenNum Int
    | TokenSym String
    | TokenEq
    | TokenAdd
    | TokenSub
    | TokenLParen
    | TokenRParen
    | TokenEOF
    deriving (Eq, Show)

scanTokens :: String -> Except String [Token]
scanTokens str = go ('\n', [], str) where 
    go inp @ (_, _bs, str) =
        case alexScan inp 0 of
            AlexEOF -> 
                return []
            AlexError _ -> 
                throwError "Invalid lexeme."
            AlexSkip  inp' len -> 
                go inp'
            AlexToken inp' len act -> do
                res <- go inp'
                return $ act (take len str) : res

}
