module Syntax where

type Name = String

data Module
  = Mod [Attribute] [Option] [Stmt]
  deriving (Show, Eq)

data Attribute
  = Attribute Name Lit
  deriving (Show, Eq, Ord)

data Option
  = OptionExplicit
  deriving (Show, Eq, Ord)

data Stmt
  = Var Name
  | Lit Lit
--   | Op Binop Expr Expr
  deriving (Eq,Show)

data Lit
  = LInt Int
  | LBool Bool
  | LString String
  deriving (Show, Eq, Ord)

data Binop = Add | Sub | Mul | Eql
  deriving (Eq, Ord, Show)
