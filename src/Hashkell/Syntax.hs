module Hashkell.Syntax where

type Name = String

type Prog = [Decl]

data Decl
    = Func Name [Name] Expr
    | Cplx Name Expr
    | Type Name [Type]
    deriving (Eq, Show)

data Type
    = Int
    | Bool
    | List Type
    deriving (Eq, Show)

data Expr
    = If Expr Expr Expr
    | Let [Def] Expr
    | App Expr Expr
    | Var Name
    | Lit Lit
    | Op BinOp Expr Expr
    deriving (Eq, Ord, Show)


data Def
    = Def Name Expr
    deriving (Eq, Ord, Show)

data Lit
    = LInt Int
    | LBool Bool
    | LList [Expr]
    deriving (Eq, Ord, Show)

data BinOp = Add | Sub | Mul | Div | Exp | EQ | LT | GT | LTE | GTE | And | Or | Cons
    deriving (Eq, Ord, Show)