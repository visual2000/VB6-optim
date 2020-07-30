module Syntax where

type Name = String

data Module
  = Mod [Attribute] [Option] [TypeDef] [FuncDecl]
  deriving (Show, Eq)

data FuncDecl
  = FuncDecl Visibility
             Name
             [FuncArgDecl]
             TypeRef
             [Stmt]
  deriving (Show, Eq)

type FuncArgDecl = TypeField -- just a synonym for formatting purposes

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
  deriving (Show, Eq)

data Option
  = OptionExplicit
  deriving (Show, Eq, Ord)

data Stmt
  = StmtDecl Name TypeRef
  | StmtAssign Lhs Expr
  -- | ECond eventually
  deriving (Eq,Show)

data Lhs
  = NameLhs Name -- just a variable
  | FieldLhs [Name] -- a.b.c = ..
  | ArrayLhs Name Int -- a[0] = 3
  deriving (Show, Eq)

data Expr
  = ELit Lit
  | EVar Name
  | EOp Binop Expr Expr
  -- todo funcall
  deriving (Show, Eq)

data Lit
  = LInt Int
  | LBool Bool
  | LString String
  deriving (Show, Eq)

data Binop = Add | Sub | Mul | Eql
  deriving (Eq, Ord, Show)
