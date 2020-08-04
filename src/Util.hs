module Util where

import Text.Regex
import AG

eolsToCRLF :: String -> String
eolsToCRLF i = subRegex (mkRegex "\n") i "\r\n"

getDllFuncRefs :: [Declaration] -> [Declaration]
getDllFuncRefs ds = [ d | d@DllFunc {} <- ds ]

getUserTypeDecls :: [Declaration] -> [Declaration]
getUserTypeDecls ds = [ d | d@(UserTypeDecl {}) <- ds ]

getGlobalVarDecls :: [Declaration] -> [Declaration]
getGlobalVarDecls ds = [ d | d@(GlobalVarDecl {}) <- ds ]

getFuncDecls :: [Declaration] -> [Declaration]
getFuncDecls ds = [ d | d@(FuncDecl {}) <- ds ]

getSubDecls :: [Declaration] -> [Declaration]
getSubDecls ds = [ d | d@(SubDecl {}) <- ds ]
