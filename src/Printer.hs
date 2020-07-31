{-# LANGUAGE FlexibleInstances #-}
module Printer where

import Text.PrettyPrint
import Prelude hiding ((<>), GT, LT)

import Syntax

class Printable a where
  pp :: a -> Doc

instance Printable Module where
  pp (Mod as os decls)
    =  foldr ($+$) empty
                            [ pp as
                            , blank
                            , pp os
                            , blank
                            , pp decls
                            , blank
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


instance Printable [Declaration] where
  pp [] = empty
  pp ((SubDecl v n args subss) : sds) =
    text (show v)
    <+> text "Sub"
    <+> text n
    <> parens (hcat $ punctuate (text ", ") (map pp args))
    $+$ nest 4 (pp subss)
    $+$ text "End Sub"
    $+$ pp sds
  pp ((GlobalVarDecl v n t) : gds) =
    text (show v)
    <+> text n
    <+> text "As"
    <+> pp t
    $+$ pp gds
  pp ((TypeDef v n fs) : ts) = (text (show v) <+> text "Type" <+> text n)
                               $+$ nest 4 (vcat (map pp fs))
                               $+$ text "End Type"
                               $+$ pp ts
  pp ((FuncDecl v n args ty ss) : fs)
    = text (show v) <+> text "Function" <+> text n
      <> parens (hcat $ punctuate (text ", ") (map pp args))
      <+> text "As" <+> pp ty
      $+$ nest 4 (pp ss)
      $+$ text "End Function"
      $+$ pp fs
  pp ((DllFuncReference v n lib args ty) : frs)
    = pp frs

instance Printable ArgumentRef where
  pp (ByRef t) = text "ByRef" <+> pp t
  pp (ByVal t) = text "ByVal" <+> pp t

instance Printable TypeField where
  pp (TypeField n ref) = text n <+> text "As" <+> pp ref
  pp (TypeFieldArray n ref) = text n <> text "()" <+> text "As" <+> pp ref

instance Printable [WithAssignment] where
  pp [] = empty
  pp ((WithAssignment l e):ws) = char '.' <> pp l
                                 <+> char '='
                                 <+> pp e
                                 $+$ pp ws

instance Printable [Stmt] where
  pp [] = empty
  pp (StmtReturn : ss) = text "Exit Function"
                         $+$ pp ss
  pp ((StmtDecl typefields):ss) = text "Dim"
                            <+> hcat (punctuate (text ", ") (map pp typefields))
                            $+$ pp ss
  pp ((StmtWith l as):ss) = text "With" <+> pp l
                            $+$ nest 4 (pp as)
                            $+$ text "End With"
                            $+$ pp ss
  pp ((StmtAssign l expr):ss) = pp l
                                <+> equals
                                <+> pp expr
                                $+$ pp ss
  pp ((StmtNakedFunctionCall l args):ss) =
    pp l
    <+> hcat (punctuate (text ", ") (map pp args))
    $+$ pp ss
  pp ((StmtIfThenElse cond ifss []):ss) = text "If"
                                              <+> pp cond
                                              <+> text "Then"
                                              $+$ nest 4 (pp ifss)
                                              $+$ text "End If"
                                              $+$ pp ss
  pp ((StmtIfThenElse cond ifss elsess):ss) = text "If"
                                              <+> pp cond
                                              <+> text "Then"
                                              $+$ nest 4 (pp ifss)
                                              $+$ text "Else"
                                              $+$ nest 4 (pp elsess)
                                              $+$ text "End If"
                                              $+$ pp ss
  pp ((StmtFor loopvar from to step bodyss):ss) = text "For"
                                                  <+> text loopvar
                                                  <+> equals
                                                  <+> pp from
                                                  <+> text "To"
                                                  <+> pp to
                                                  <+> text "Step"
                                                  <+> pp step
                                                  $+$ nest 4 (pp bodyss)
                                                  $+$ text "Next" <+> text loopvar
                                                  $+$ pp ss
  pp ((StmtWhileLoop stmts expr):ss) = text "Do"
                                       $+$ nest 4 (pp stmts)
                                       $+$ text "Loop While"
                                       <+> pp expr
                                       $+$ pp ss

instance Printable Lhs where
  pp (NameLhs n) = text n
  pp (FieldLhs n lhs) = text n <> char '.' <> pp lhs
  pp (ArrayLhs n i) = text n <> lparen <> int i <> rparen

instance Printable Expr where
  pp (ELit l) = pp l
  pp (EVar n) = text n
  pp (ECall lhs args) = pp lhs
                        <> lparen
                        <> hcat (punctuate (text ", ") (map pp args))
                        <> rparen
  pp (EAccess ns) = hcat $ punctuate (char '.') (map text ns)
  pp (EOp b e1 e2) = lparen <> pp e1 <+> pp b <+> pp e2 <> rparen
  pp (ENeg e) = char '-' <> pp e

instance Printable Binop where
  pp Add = char '+'
  pp Sub = char '-'
  pp Mul = char '*'
  pp Div = char '/'
  pp GT  = char '>'
  pp LT  = char '<'
  pp GEQ = text ">="
  pp LEQ = text "<="
  pp And = text "And"
  pp Or  = text "Or"
  pp Eql = equals -- todo hmm can VB do this?

instance Printable Lit where
  pp (LInt i) = int i
  pp (LBool b) = text $ show b
  pp (LString s) = doubleQuotes $ text s
  pp (LDouble d) = double d

instance Printable TypeRef where
  pp TDouble = text "Double"
  pp TInt = text "Integer"
  pp TString = text "String"
  pp TBoolean = text "Boolean"
  pp (TUDT n) = text n
