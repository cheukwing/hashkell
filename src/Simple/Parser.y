{
-- Adapted from Write You A Haskell
module Simple.Parser (
    parseProg
) where

import Simple.Lexer
import Simple.Syntax

import Control.Monad.Except

}

%tokentype { Token }

%token
    if    { TokenIf }
    then  { TokenThen }
    else  { TokenElse }
    let   { TokenLet }
    in    { TokenIn }
    NUM   { TokenNum $$ }
    VAR   { TokenSym $$ }
    '='   { TokenDef }
    '=='  { TokenEq }
    '+'   { TokenAdd }
    '-'   { TokenSub }
    '('   { TokenLParen }
    ')'   { TokenRParen }
    '{'   { TokenLBrace }
    '}'   { TokenRBrace }
    ';'   { TokenEnd }

%monad { Except String } { (>>=) } { return }
%error { parseError }

%name prog

%left '=='
%left '+' '-'
%%

Prog : {- empty -}                 { [] }
     | Func ';' Prog               { $1 : $3 }

Func : VAR Args '=' Expr           { Func $1 $2 $4 }

Args : {- empty -}                 { [] }
     | VAR Args                    { $1 : $2 }

Expr : if Expr then Expr else Expr { If $2 $4 $6 }
     | let '{' Defs '}' in Expr    { Let $3 $6 }     
     | Form                        { $1 }

Defs : {- empty -}                 { [] }
     | VAR '=' Expr ';' Defs       { (Def $1 $3) : $5 }

Form : Form '+' Form               { Op Add $1 $3 }
     | Form '-' Form               { Op Sub $1 $3 }
     | Form '==' Form              { Op Eq $1 $3 }
     | Fact                        { $1 }

Fact : Fact Atom                   { App $1 $2 }
     | Atom                        { $1 }

Atom : '(' Expr ')'                { $2 }
     | NUM                         { Lit (LInt $1) }
     | VAR                         { Var $1 }

{
parseError :: [Token] -> Except String a
parseError (l : ls) 
    = throwError (show l)
parseError [] 
    = throwError "Unexpected end of Input"

parseProg :: String -> Either String Prog
parseProg input = runExcept $ do
  tokenStream <- scanTokens input
  prog tokenStream

}
