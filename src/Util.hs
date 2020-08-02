module Util where

import Text.Regex
import Syntax

eolsToCRLF :: String -> String
eolsToCRLF i = subRegex (mkRegex "\n") i "\r\n"

getDllFuncRefs :: [Declaration] -> [Declaration]
getDllFuncRefs ds = [ d | d@DllFunc {} <- ds ]

getUserTypeDecls :: [Declaration] -> [Declaration]
getUserTypeDecls ds = [ d | d@(UserTypeDecl {}) <- ds ]

getGlobalVarDecls ds = [ d | d@(GlobalVarDecl {}) <- ds ]

getFuncDecls ds = [ d | d@(FuncDecl {}) <- ds ]

getSubDecls ds = [ d | d@(SubDecl {}) <- ds ]
