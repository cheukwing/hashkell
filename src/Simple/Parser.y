{
-- Adapted from Write You A Haskell
module Simple.Parser (
    parseExpr
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
    NUM   { TokenNum $$ }
    VAR   { TokenSym $$ }
    '=='  { TokenEq }
    '+'   { TokenAdd }
    '-'   { TokenSub }
    '('   { TokenLParen }
    ')'   { TokenRParen }

%monad { Except String } { (>>=) } { return }
%error { parseError }

%name expr

%left '=='
%left '+' '-'
%%

Expr : if Expr then Expr else Expr { If $2 $4 $6 }
     | Form                        { $1 }

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
parseError (l:ls) = throwError (show l)
parseError [] = throwError "Unexpected end of Input"

parseExpr :: String -> Either String Expr
parseExpr input = runExcept $ do
  tokenStream <- scanTokens input
  expr tokenStream

}
