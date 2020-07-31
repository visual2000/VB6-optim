{-# LANGUAGE FlexibleInstances #-}
module Printer where

import Text.PrettyPrint
import Prelude hiding ((<>))

import Syntax

class Printable a where
  pp :: a -> Doc

instance Printable Module where
  pp (Mod as os udts funcs) =  foldr ($+$) empty
                            [ pp as
                            , blank
                            , pp os
                            , blank
                            , pp udts
                            , blank
                            , pp funcs
                            , text "' The end"
                            ]

blank = text "' ---"

instance Printable [Attribute] where
  pp [] = empty
  pp ((Attribute n lit) : as) = text "Attribute" <+> text n <+> equals <+> pp lit
                                $+$ pp as

instance Printable [Option] where
  pp [] = empty
  pp (OptionExplicit : os) = text "Option Explicit"
                             $+$ pp os


instance Printable [TypeDef] where
  pp [] = empty
  pp ((TypeDef v n fs) : ts) = (text (show v) <+> text "Type" <+> text n)
                               $+$ nest 4 (vcat (map pp fs))
                               $+$ text "End Type"
                               $+$ pp ts

instance Printable TypeField where
  pp (TypeField n ref) = text n <+> text "As" <+> pp ref
  pp (TypeFieldArray n ref) = text n <> text "()" <+> text "As" <+> pp ref

instance Printable [Stmt] where
  pp [] = empty
  pp ((StmtDecl typefields):ss) = text "Dim"
                            <+> hcat (punctuate (text ", ") (map pp typefields))
                            $+$ pp ss
  pp ((StmtAssign l expr):ss) = pp l
                                <+> equals
                                <+> pp expr
                                $+$ pp ss
  pp ((StmtIfThenElse cond ifss elsess):ss) = text "IFTHENELSE"
                                              $+$ pp ss
  pp ((StmtFor loopvar from to step bodyss):ss) = text "FORNEXT"
                                                  $+$ pp ss

instance Printable Lhs where
  pp (NameLhs n) = text n
  pp (FieldLhs ns) = hcat $ punctuate (char '.') (map text ns)
  pp (ArrayLhs n i) = text n <> lparen <> int i <> rparen

instance Printable Expr where
  pp (ELit l) = pp l
  pp (EVar n) = text n
  pp (ECall lhs args) = text "CALL"
  pp (EAccess ns) = text "DOT-ACCESS"
  pp (EOp b e1 e2) = lparen <> pp e1 <+> pp b <+> pp e2 <> rparen

instance Printable Binop where
  pp Add = char '+'
  pp Sub = char '-'
  pp Mul = char '*'
  pp Eql = char '=' -- todo hmm can VB do this?

instance Printable [FuncDecl] where
  pp [] = empty
  pp ((FuncDecl v n args ty ss) : fs)
    = text (show v) <+> text "Function" <+> text n
      <> parens (hcat $ punctuate (text ", ") (map pp args))
      <+> text "As" <+> pp ty
      $+$ nest 4 (pp ss)
      $+$ text "End Function"
      $+$ pp fs

instance Printable Lit where
  pp (LInt i) = int i
  pp (LBool b) = text $ show b
  pp (LString s) = doubleQuotes $ text s

instance Printable TypeRef where
  pp TDouble = text "Double"
  pp TInt = text "Integer"
  pp TString = text "String"
  pp TBoolean = text "Boolean"
  pp (TUDT n) = text n
