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

instance Printable [FuncDecl] where
  pp [] = empty
  pp ((FuncDecl v n args ty ss) : fs)
    = text (show v) <+> text "Function" <+> text n
      <> parens (hcat $ punctuate (text ", ") (map pp args))
      <+> text "As" <+> pp ty
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
  pp (TUDT n) = text n
