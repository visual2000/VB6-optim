module Printer where

import Text.PrettyPrint

import Syntax (Module)

toDoc :: Module -> Doc
toDoc _ = text "foo"
