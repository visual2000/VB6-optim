{-# LANGUAGE FlexibleInstances #-}
module Printer where

import Text.PrettyPrint
import Prelude hiding ((<>))

import Util
import AG

printModule :: Module -> String
printModule m = eolsToCRLF $ (render . pp) m ++ "\n"

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

blank :: Doc
blank = text "' ---"

instance Printable [ModuleAttribute] where
  pp [] = empty
  pp ((ModuleAttribute n lit) : as) = text "Attribute" <+> text n <+> equals <+> pp lit
                                $+$ pp as

instance Printable [ModuleOption] where
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
  pp ((GlobalVarDecl v d) : gds) =
    text (show v)
    <+> pp d
    $+$ pp gds
  pp ((UserTypeDecl v n fs) : ts) = (text (show v) <+> text "Type" <+> text n)
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
  pp ((DllFunc v n lib args ty) : frs)
    = text (show v) <+> text "Declare" <+> text "Function"
      <+> text n
      <+> text "Lib"
      <+> doubleQuotes (text lib)
      <+> parens (hcat $ punctuate (text ", ") (map pp args))
      <+> text "As"
      <+> pp ty
      $+$ pp frs

instance Printable FuncArgDecl where
  pp (Unspecified t) = pp t
  pp (ByRef t)       = text "ByRef" <+> pp t
  pp (ByVal t)       = text "ByVal" <+> pp t

instance Printable FuncArgDeclField where
  pp (FuncArgDeclField n ref) = text n <+> text "As" <+> pp ref
  pp (FuncArgDeclFieldArray n ref) = text n <> text "()" <+> text "As" <+> pp ref

instance Printable UserTypeDeclField where
  pp (UserTypeDeclField n ref) = text n <+> text "As" <+> pp ref
  pp (UserTypeDeclFieldArray n ref) = text n <> text "()" <+> text "As" <+> pp ref
  pp (UserTypeDeclFieldArrayWithUpperBound n b ref) = text n <> text "("
                                              <> int b
                                              <> text ")"
                                              <+> text "As" <+> pp ref

instance Printable GlobalTypeDecl where
  pp (GlobalTypeDecl n ref) = text n <+> text "As" <+> pp ref
  pp (GlobalTypeDeclArray n ref) = text n <> text "()" <+> text "As" <+> pp ref
  pp (GlobalTypeDeclArrayWithUpperBound n b ref) = text n <> text "("
                                              <> int b
                                              <> text ")"
                                              <+> text "As" <+> pp ref
  pp (GlobalTypeDeclArrayWithBounds n l u ref) = text n <> text "("
                                            <> int l <+> text "To" <+> int u
                                            <> text ")"
                                            <+> text "As" <+> pp ref

instance Printable StmtTypeDecl where
  pp (StmtTypeDecl n ref) = text n <+> text "As" <+> pp ref
  pp (StmtTypeDeclArray n ref) = text n <> text "()" <+> text "As" <+> pp ref
  pp (StmtTypeDeclArrayWithUpperBound n b ref) = text n <> text "("
                                              <> int b
                                              <> text ")"
                                              <+> text "As" <+> pp ref
  pp (StmtTypeDeclArrayWithBounds n l u ref) = text n <> text "("
                                            <> int l <+> text "To" <+> int u
                                            <> text ")"
                                            <+> text "As" <+> pp ref

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
  pp ((StmtSetAssign l expr):ss) = text "Set"
                                   <+> pp l
                                   <+> equals
                                   <+> pp expr
                                   $+$ pp ss
  pp ((StmtLSetAssign l expr):ss) = text "LSet"
                                    <+> pp l
                                    <+> equals
                                    <+> pp expr
                                    $+$ pp ss
  pp ((StmtCall l args):ss) =
    text "Call"
    <+> pp l
    <+> parens (hcat (punctuate (text ", ") (map pp args)))
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
  pp ((StmtFor loopvar from to Nothing bodyss):ss) = text "For"
                                                  <+> text loopvar
                                                  <+> equals
                                                  <+> pp from
                                                  <+> text "To"
                                                  <+> pp to
                                                  $+$ nest 4 (pp bodyss)
                                                  $+$ text "Next" <+> text loopvar
                                                  $+$ pp ss
  pp ((StmtFor loopvar from to (Just step) bodyss):ss) = text "For"
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
  pp ((StmtDoStatementsLoopWhileCond stmts expr):ss) = text "Do"
                                       $+$ nest 4 (pp stmts)
                                       $+$ text "Loop While"
                                       <+> pp expr
                                       $+$ pp ss

instance Printable Lhs where
  pp (NameLhs n) = text n
  pp (FieldLhs lhss) = hcat (punctuate (char '.') (map pp lhss))
  pp (ArrayLhs n is) = text n <> lparen <> hcat (punctuate (char ',') (map pp is)) <> rparen

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
  pp GrT  = char '>'
  pp LTh  = char '<'
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
