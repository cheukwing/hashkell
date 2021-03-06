{
-- Adapted from Write You A Haskell
{-# LANGUAGE FlexibleContexts #-}

module Hashkell.Lexer (
    Token(..),
    scanTokens,
) where

import Hashkell.Syntax

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

    "##"                          { \s -> TokenComplexity }
    "::"                          { \s -> TokenType }

    "Int"                         { \s -> TokenInt }
    "Bool"                        { \s -> TokenBool }
    "->"                          { \s -> TokenArrow }

    if                            { \s -> TokenIf }
    then                          { \s -> TokenThen }
    else                          { \s -> TokenElse }
    let                           { \s -> TokenLet }
    in                            { \s -> TokenIn }
    True                          { \s -> TokenTrue }
    False                         { \s -> TokenFalse }
    $digit+                       { \s -> TokenNum (read s) }
    $lower [$alpha $digit \_ \']* { \s -> TokenSym s }
    "="                           { \s -> TokenDef }

    "=="                          { \s -> TokenEQ }
    "<"                           { \s -> TokenLT }
    ">"                           { \s -> TokenGT }
    "<="                          { \s -> TokenLTE }
    ">="                          { \s -> TokenGTE }
    "&&"                          { \s -> TokenAnd }
    "||"                          { \s -> TokenOr }
    [\+]                          { \s -> TokenAdd }
    [\-]                          { \s -> TokenSub }
    [\*]                          { \s -> TokenMul }
    [\/]                          { \s -> TokenDiv }
    [\^]                          { \s -> TokenExp}
    ":"                           { \s -> TokenCons }

    \(                            { \s -> TokenLParen }
    \)                            { \s -> TokenRParen }
    \{                            { \s -> TokenLBrace }
    \}                            { \s -> TokenRBrace }
    \[                            { \s -> TokenLBrack }
    \]                            { \s -> TokenRBrack }
    ","                           { \s -> TokenSep }
    ";"                           { \s -> TokenEnd }

{
data Token
    = TokenComplexity
    | TokenType
    | TokenInt
    | TokenBool
    | TokenArrow
    | TokenIf
    | TokenThen
    | TokenElse
    | TokenLet
    | TokenIn
    | TokenTrue
    | TokenFalse
    | TokenNum Int
    | TokenSym String
    | TokenDef
    | TokenEQ     | TokenLT     | TokenGT     | TokenLTE | TokenGTE | TokenAnd | TokenOr
    | TokenAdd    | TokenSub    | TokenMul    | TokenDiv | TokenExp
    | TokenCons
    | TokenLParen | TokenRParen | TokenLBrace | TokenRBrace | TokenLBrack | TokenRBrack
    | TokenSep
    | TokenEnd
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
