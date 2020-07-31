module Syntax where

import Prelude hiding ((<), (>), LT, GT)

type Name = String

data Module
  = Mod [Attribute] [Option] [TypeDef] [FuncDecl]
  deriving (Show, Eq)

data FuncDecl
  = FuncDecl Visibility
             Name
             [TypeField]
             TypeRef
             [Stmt]
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
  | TypeFieldArray Name TypeRef
  deriving (Show, Eq)

data TypeRef
  = TDouble
  | TInt
  | TBoolean
  | TString
  | TUDT Name
  deriving (Show, Eq)

data Attribute
  = Attribute Name Lit
  deriving (Show, Eq)

data Option
  = OptionExplicit
  deriving (Show, Eq, Ord)

data Stmt
  = StmtDecl [TypeField]
  | StmtReturn
  | StmtAssign Lhs Expr
  | StmtIfThenElse Expr [Stmt] [Stmt]
  | StmtFor Name Expr Expr Int [Stmt]
  deriving (Eq,Show)

data Lhs
  = NameLhs Name -- just a variable
  | FieldLhs [Name] -- a.b.c = ..
  | ArrayLhs Name Int -- a[0] = 3
  deriving (Show, Eq)

data Expr
  = ELit Lit
  | EVar Name
  | ECall Lhs [Expr]
  | EAccess [Name] -- a.b.c
  | EOp Binop Expr Expr
  | ENeg Expr
  -- todo funcall
  deriving (Show, Eq)

data Lit
  = LInt Int
  | LBool Bool
  | LString String
  deriving (Show, Eq)

data Binop = Add | Sub | Mul | Div | Eql
           | GT | LT | And | Or
  deriving (Eq, Ord, Show)
