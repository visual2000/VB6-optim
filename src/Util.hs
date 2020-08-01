module Util where

import Text.Regex
import Syntax

fixDOSEOL :: String -> String
fixDOSEOL = \i -> subRegex (mkRegex "\n") i "\r\n"

getDllFuncRefs :: [Declaration] -> [Declaration]
getDllFuncRefs ds = [ d | d@(DllFuncReference _ _ _ _ _) <- ds ]

getTypeDefs :: [Declaration] -> [Declaration]
getTypeDefs ds = [ d | d@(TypeDef _ _ _) <- ds ]

getGlobalVarDecls ds = [ d | d@(GlobalVarDecl _ _ _) <- ds ]

getFuncDecls ds = [ d | d@(FuncDecl _ _ _ _ _) <- ds ]

getSubDecls ds = [ d | d@(SubDecl _ _ _ _) <- ds ]
