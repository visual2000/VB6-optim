module Util where

import Text.Regex
import Syntax

eolsToCRLF :: String -> String
eolsToCRLF = \i -> subRegex (mkRegex "\n") i "\r\n"

getDllFuncRefs :: [Declaration] -> [Declaration]
getDllFuncRefs ds = [ d | d@(DllFunc _ _ _ _ _) <- ds ]

getUserTypeDecls :: [Declaration] -> [Declaration]
getUserTypeDecls ds = [ d | d@(UserTypeDecl _ _ _) <- ds ]

getGlobalVarDecls ds = [ d | d@(GlobalVarDecl _ _ _) <- ds ]

getFuncDecls ds = [ d | d@(FuncDecl _ _ _ _ _) <- ds ]

getSubDecls ds = [ d | d@(SubDecl _ _ _ _) <- ds ]
