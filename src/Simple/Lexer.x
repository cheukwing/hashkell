{
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
$alpha = [a-zA-Z]
$eol   = [\n]

tokens :-
    $eol                          ;
    $white+                       ;

    "--".*                        ;

    $digit+                       { \s -> TokenNum (read s) }
    $alpha [$alpha $digit \_ \']* { \s -> TokenSym s }
    "=="                          { \s -> TokenEq }
    [\+]                          { \s -> TokenAdd }
    [\-]                          { \s -> TokenSub }
    \(                            { \s -> TokenLParen }
    \)                            { \s -> TokenRParen }
    if                            { \s -> TokenIf }
    then                          { \s -> TokenThen }
    else                          { \s -> TokenElse }

{
data Token
    = TokenNum Int
    | TokenSym String
    | TokenEq
    | TokenAdd
    | TokenSub
    | TokenLParen
    | TokenRParen
    | TokenIf
    | TokenThen
    | TokenElse
    | TokenEOF
    deriving (Eq, Show)

scanTokens :: String -> Except String [Token]
scanTokens str = go ('\n',[],str) where 
    go inp@(_,_bs,str) =
        case alexScan inp 0 of
            AlexEOF -> return []
            AlexError _ -> throwError "Invalid lexeme."
            AlexSkip  inp' len     -> go inp'
            AlexToken inp' len act -> do
                res <- go inp'
                let rest = act (take len str)
                return (rest : res)

}
