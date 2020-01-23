module Simple.Syntax where

type Name = String

data Expr
    = If Expr Expr Expr
    | App Expr Expr
    | Var Name
    | Lit Lit
    | Op BinOp Expr Expr
    deriving (Eq, Show)

data Lit
    = LInt Int
    deriving (Eq, Show, Ord)

data BinOp = Add | Sub | Eq
    deriving (Eq, Show, Ord)