-- Adapted from Write You A Haskell
module Simple.Syntax where

type Name = String

type Prog = [Decl]

data Decl
    = Func Name [Name] Expr
    | Cplx Name Expr
    deriving (Eq, Show)

data Expr
    = If Expr Expr Expr
    | Let [Def] Expr
    | App Expr Expr
    | Var Name
    | Lit Lit
    | Op BinOp Expr Expr
    deriving (Eq, Show)

data Def
    = Def Name Expr
    deriving (Eq, Show)

data Lit
    = LInt Int
    | LBool Bool
    deriving (Eq, Show, Ord)

data BinOp = Add | Sub | Mul | Div | Exp | EQ | LT | GT | LTE | GTE | And | Or
    deriving (Eq, Show, Ord)