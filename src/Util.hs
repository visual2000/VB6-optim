module Util where

import Text.Regex

fixDOSEOL :: String -> String
fixDOSEOL = \i -> subRegex (mkRegex "\n") i "\r\n"
