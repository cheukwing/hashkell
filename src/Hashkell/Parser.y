{
-- Adapted from Write You A Haskell
module Hashkell.Parser (
    parseProg
) where

import Prelude hiding (EQ, GT, LT)
import Hashkell.Lexer
import Hashkell.Syntax

import Control.Monad.Except

}

%tokentype { Token }

%token
    '##'  { TokenComplexity }
    '::'  { TokenType }
    Int   { TokenInt }
    Bool  { TokenBool}
    '->'  { TokenArrow }
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
    '^'   { TokenExp }
    ':'   { TokenCons }
    '('   { TokenLParen }
    ')'   { TokenRParen }
    '{'   { TokenLBrace }
    '}'   { TokenRBrace }
    '['   { TokenLBrack }
    ']'   { TokenRBrack }
    ';'   { TokenEnd }
    ','   { TokenSep }

%monad { Except String } { (>>=) } { return }
%error { parseError }

%name prog

%right '||'
%right '&&'
%right ':'
%nonassoc '==' '>' '<' '<=' '>='
%left '+' '-'
%left '*' '/'
%left '^'
%%

Prog : {- empty -}                 { [] }
     | Decl                        { [ $1 ] }
     | Decl ';' Prog               { $1 : $3 }

Decl : VAR Args '=' Expr           { Func $1 $2 $4 }
     | VAR '##' Expr               { Cplx $1 $3 }
     | VAR '::' Type               { Type $1 $3 }

Args : {- empty -}                 { [] }
     | VAR Args                    { $1 : $2 }

Type : T                           { [ $1 ] }
     | T '->' Type                 { $1 : $3 }

T    : Int                         { Int }
     | Bool                        { Bool }
     | '[' T ']'                   { List $2 }

Expr : if Expr then Expr else Expr { If $2 $4 $6 }
     | let '{' Defs '}' in Expr    { Let $3 $6 }     
     | let Defs in Expr            { Let $2 $4 }     
     | Form                        { $1 }

Defs : {- empty -}                 { [] }
     | VAR '=' Expr                { [ Def $1 $3 ] }
     | VAR '=' Expr ';' Defs       { (Def $1 $3) : $5 }

Form : Form '+' Form               { Op Add $1 $3 }
     | Form '-' Form               { Op Sub $1 $3 }
     | Form '*' Form               { Op Mul $1 $3 }
     | Form '/' Form               { Op Div $1 $3 }
     | Form '^' Form               { Op Exp $1 $3 }
     | Form '==' Form              { Op EQ $1 $3 }
     | Form '<' Form               { Op LT $1 $3 }
     | Form '>' Form               { Op GT $1 $3 }
     | Form '<=' Form              { Op LTE $1 $3 }
     | Form '>=' Form              { Op GTE $1 $3 }
     | Form '&&' Form              { Op And $1 $3 }
     | Form '||' Form              { Op Or $1 $3 }
     | Form ':' Form               { Op Cons $1 $3 }
     | Fact                        { $1 }

Fact : Fact Atom                   { App $1 $2 }
     | Atom                        { $1 }

Atom : '(' Expr ')'                { $2 }
     | NUM                         { Lit (LInt $1) }
     | VAR                         { Var $1 }
     | True                        { Lit (LBool True) }
     | False                       { Lit (LBool False) }
     | '[' ']'                     { Lit (LList []) }
     | '[' List ']'                { Lit (LList $2) }

List : Expr                        { [ $1 ] }
     | Expr ',' List               { $1 : $3 }


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
