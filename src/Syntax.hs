module Syntax where

import Prelude hiding ((<), (>), LT, GT)

type Name = String

data Module = Mod [Attribute]
                  [Option]
                  [Declaration]
  deriving (Show, Eq)

data Declaration
  = DllFuncReference Visibility Name String [ArgumentRef] TypeRef
  | TypeDef Visibility Name [TypeField]
  | GlobalVarDecl Visibility Name TypeRef
  | FuncDecl Visibility Name [ArgumentRef] TypeRef [Stmt]
  | SubDecl  Visibility Name [ArgumentRef]         [Stmt]
  deriving (Show, Eq)

data Visibility
  = Public
  | Private
  deriving (Show, Eq)

data ArgumentRef
  = ByVal TypeField
  | ByRef TypeField
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

data WithAssignment
  = WithAssignment Lhs Expr
  deriving (Show, Eq)

data Stmt
  = StmtDecl [TypeField]
  | StmtReturn
  | StmtWith Lhs [WithAssignment]
  | StmtAssign Lhs Expr
  | StmtNakedFunctionCall Lhs [Expr]
  | StmtIfThenElse Expr [Stmt] [Stmt]
  | StmtFor Name Expr Expr Expr [Stmt]
  deriving (Eq,Show)

data Lhs
  = NameLhs Name -- just a variable
  | FieldLhs Name Lhs -- a.b.c = ..
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
  | LDouble Double
  deriving (Show, Eq)

data Binop = Add | Sub | Mul | Div | Eql
           | GT | LT | And | Or
  deriving (Eq, Ord, Show)
