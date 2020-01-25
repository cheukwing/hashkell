{
-- Adapted from Write You A Haskell
module Simple.Parser (
    parseProg
) where

import Prelude hiding (EQ, GT, LT)
import Simple.Lexer
import Simple.Syntax

import Control.Monad.Except

}

%tokentype { Token }

%token
    '#'   { TokenComplexity }
    '|'   { TokenDelimiter }
    '2^n' { TokenExponential }
    POLY  { TokenPolynomial $$ }
    if    { TokenIf }
    then  { TokenThen }
    else  { TokenElse }
    let   { TokenLet }
    in    { TokenIn }
    True  { TokenTrue }
    False { TokenFalse }
    NUM   { TokenNum $$ }
    VAR   { TokenSym $$ }
    '='   { TokenDef }
    '=='  { TokenEQ }
    '<'   { TokenLT }
    '>'   { TokenGT }
    '<='  { TokenLTE }
    '>='  { TokenGTE }
    '&&'  { TokenAnd }
    '||'  { TokenOr }
    '+'   { TokenAdd }
    '-'   { TokenSub }
    '*'   { TokenMul }
    '/'   { TokenDiv }
    '('   { TokenLParen }
    ')'   { TokenRParen }
    '{'   { TokenLBrace }
    '}'   { TokenRBrace }
    ';'   { TokenEnd }

%monad { Except String } { (>>=) } { return }
%error { parseError }

%name prog

%right '||'
%right '&&'
%nonassoc '==' '>' '<' '<=' '>='
%left '+' '-'
%left '*' '/'
%%

Prog : {- empty -}                 { [] }
     | Decl ';' Prog               { $1 : $3 }

Decl : VAR Args '=' Expr           { Func (FuncData $1 $2 $4) }
     | '#' VAR Cplx                { Complexity $2 $3 }

Cplx : {- empty -}                 { [] }
     | '|' Cplx                    { None : $2 }
     | '|' '2^n' Cplx              { Exponential : $3 }
     | '|' POLY Cplx               { Polynomial $2 : $3 }

Args : {- empty -}                 { [] }
     | VAR Args                    { $1 : $2 }

Expr : if Expr then Expr else Expr { If $2 $4 $6 }
     | let '{' Defs '}' in Expr    { Let $3 $6 }     
     | Form                        { $1 }

Defs : {- empty -}                 { [] }
     | VAR '=' Expr ';' Defs       { (Def $1 $3) : $5 }

Form : Form '+' Form               { Op Add $1 $3 }
     | Form '-' Form               { Op Sub $1 $3 }
     | Form '*' Form               { Op Mul $1 $3 }
     | Form '/' Form               { Op Div $1 $3 }
     | Form '==' Form              { Op EQ $1 $3 }
     | Form '<' Form               { Op LT $1 $3 }
     | Form '>' Form               { Op GT $1 $3 }
     | Form '<=' Form              { Op LTE $1 $3 }
     | Form '>=' Form              { Op GTE $1 $3 }
     | Form '&&' Form              { Op And $1 $3 }
     | Form '||' Form              { Op Or $1 $3 }
     | Fact                        { $1 }

Fact : Fact Atom                   { App $1 $2 }
     | Atom                        { $1 }

Atom : '(' Expr ')'                { $2 }
     | NUM                         { Lit (LInt $1) }
     | VAR                         { Var $1 }
     | True                        { Lit (LBool True) }
     | False                       { Lit (LBool False) }


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
