module Syntax where

type Name = String

data Module
  = Mod [Attribute] [Option] [TypeDef] [Stmt]
  deriving (Show, Eq)

data TypeDef
  = TypeDef Visibility Name [TypeField]
  deriving (Show, Eq)

data Visibility
  = Public
  | Private
  deriving (Show, Eq)

data TypeField
  = TypeField Name TypeRef
  deriving (Show, Eq)

data TypeRef
  = TDouble
  | TInt
  | TString
  | TUDT Name
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
  deriving (Eq, Ord)

data Binop = Add | Sub | Mul | Eql
  deriving (Eq, Ord, Show)

instance Show Lit where
  show (LInt i) = show i
  show (LBool b) = show b
  show (LString s) = "\"" ++ s ++ "\""
