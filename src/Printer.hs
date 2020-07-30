{-# LANGUAGE FlexibleInstances #-}
module Printer where

import Text.PrettyPrint
import Prelude hiding ((<>))

import Syntax

class Printable a where
  pp :: a -> Doc

instance Printable Module where
  pp (Mod as os udts ss) =  foldr ($+$) empty
                            [ pp as
                            , blank
                            , pp os
                            , blank
                            , pp udts
                            , blank
                            , text "' The end"
                            ]

blank = text "' ---"

instance Printable [Attribute] where
  pp [] = empty
  pp ((Attribute n lit) : as) = text "Attribute" <+> text n <+>
                                  equals <+> pp lit
                                  $+$ pp as

instance Printable [Option] where
  pp [] = empty
  pp (OptionExplicit : os) = text "Option Explicit"
                               $+$ pp os


instance Printable [TypeDef] where
  pp [] = empty
  pp ((TypeDef v n fs) : ts) = (text (show v) <+> text "Type" <+> text n)
                               $+$ nest 4 (pp fs)
                               $+$ text "End Type"
                               $+$ pp ts

instance Printable [TypeField] where
  pp [] = empty
  pp ((TypeField n ref) : tfs) = text n <+>
                                 text "As" <+>
                                 pp ref $+$
                                 pp tfs

instance Printable Lit where
  pp (LInt i) = int i
  pp (LBool b) = text $ show b
  pp (LString s) = doubleQuotes $ text s

instance Printable TypeRef where
  pp TDouble = text "Double"
  pp TInt = text "Integer"
  pp TString = text "String"
  pp (TUDT n) = text n
