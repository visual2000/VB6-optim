{-# LANGUAGE FlexibleInstances #-}
module Printer where

import Text.PrettyPrint
import Prelude hiding ((<>))

import Syntax

class Printable a where
  pp :: a -> Doc

instance Printable Module where
  pp (Mod as os udts ss) = pp as <>
                           pp os

instance Printable [Attribute] where
  pp [] = text "\n"
  pp ((Attribute n lit) : as) = text "Attribute" <+> text n <+>
                                  char '=' <+> text (show lit)
                                  $+$ pp as

instance Printable [Option] where
  pp [] = text "\n"
  pp (OptionExplicit : os) = text "Option Explicit"
                               $+$ pp os
