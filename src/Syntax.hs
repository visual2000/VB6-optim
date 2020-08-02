module Syntax where

import Prelude hiding ((<), (>), LT, GT)

type Name = String

data Module = Mod [ModuleAttribute]
                  [ModuleOption]
                  [Declaration]
  deriving (Show, Eq)

data Declaration
  = DllFunc Visibility Name String [FuncArgDecl] TypeRef
  | UserTypeDecl Visibility Name [TypeDecl]
  | GlobalVarDecl Visibility Name TypeRef
  | FuncDecl Visibility Name [FuncArgDecl] TypeRef [Stmt]
  | SubDecl  Visibility Name [FuncArgDecl]         [Stmt]
  deriving (Show, Eq)

data Visibility
  = Public
  | Private
  deriving (Show, Eq)

data FuncArgDecl
  = ByVal       TypeDecl
  | ByRef       TypeDecl
  | Unspecified TypeDecl
  deriving (Show, Eq)

data TypeDecl
  = TypeDecl Name TypeRef
  | TypeDeclArray Name TypeRef
  | TypeDeclArrayWithUpperBound Name Int TypeRef
  | TypeDeclArrayWithBounds Name Int Int TypeRef
  deriving (Show, Eq)

data TypeRef
  = TDouble
  | TInt
  | TBoolean
  | TString
  | TUDT Name
  deriving (Show, Eq)

data ModuleAttribute
  = ModuleAttribute Name Lit
  deriving (Show, Eq)

data ModuleOption
  = OptionExplicit
  deriving (Show, Eq, Ord)

data WithAssignment
  = WithAssignment Lhs Expr
  deriving (Show, Eq)

data Stmt
  = StmtDecl [TypeDecl]
  | StmtReturn
  | StmtWith Lhs [WithAssignment]
  | StmtAssign Lhs Expr
  | StmtSetAssign Lhs Expr
  | StmtLSetAssign Lhs Expr
  | StmtNakedFunctionCall Lhs [Expr]
  | StmtIfThenElse Expr [Stmt] [Stmt]
  | StmtFor Name Expr Expr (Maybe Expr) [Stmt]
  | StmtDoStatementsLoopWhileCond [Stmt] Expr
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
  deriving (Show, Eq)

data Lit
  = LInt Int
  | LBool Bool
  | LString String
  | LDouble Double
  deriving (Show, Eq)

data Binop = Add | Sub | Mul | Div | Eql
           | GT | LT | GEQ | LEQ | And | Or
  deriving (Eq, Ord, Show)
