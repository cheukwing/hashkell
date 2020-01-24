-- Adapted from Write You A Haskell
module Simple.Syntax where

type Name = String

type Prog = [Func]

data Func 
    = Func Name [Name] Expr
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

newtype Lit
    = LInt Int
    deriving (Eq, Show, Ord)

data BinOp = Add | Sub | Eq
    deriving (Eq, Show, Ord)