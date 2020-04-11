
module Simple.Syntax where

import Data.List (intercalate)

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
    deriving Eq

instance Show Type where
    show Int      = "Int"
    show Bool     = "Bool"
    show (List t) = "[" ++ show t ++ "]"

data Expr
    = If Expr Expr Expr
    | Let [Def] Expr
    | App Expr Expr
    | Var Name
    | Lit Lit
    | Op BinOp Expr Expr
    deriving (Eq, Ord)

instance Show Expr where
    show (If e1 e2 e3) = "if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3
    show (Let ds e)    = "let " ++ intercalate ";" (map show ds) ++ " in " ++ show e
    show (App e1 e2)   = show e1 ++ " " ++ show e2
    show (Var n)       = n
    show (Lit l)       = show l
    show (Op op e1 e2) = "(" ++ show e1 ++ " " ++ show op ++ " " ++ show e2 ++ ")"

data Def
    = Def Name Expr
    deriving (Eq, Ord)

instance Show Def where
    show (Def n e) = n ++ " = " ++ show e

data Lit
    = LInt Int
    | LBool Bool
    | LList [Expr]
    deriving (Eq, Ord)

instance Show Lit where
    show (LInt i)   = show i
    show (LBool b)  = show b
    show (LList ls) = "[" ++ intercalate "," (map show ls) ++"]"

data BinOp = Add | Sub | Mul | Div | Exp | EQ | LT | GT | LTE | GTE | And | Or | Cons
    deriving (Eq, Ord)

instance Show BinOp where
    show Add               = "+"
    show Sub               = "-"
    show Mul               = "*" 
    show Div               = "/"
    show Exp               = "^"
    show Simple.Syntax.EQ  = "=="
    show Simple.Syntax.LT  = "<"
    show Simple.Syntax.GT  = ">"
    show LTE               = "<="
    show GTE               = ">="
    show And               = "&&"
    show Or                = "||"
    show Cons              = ":"