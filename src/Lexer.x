-- -*- mode: prog -*-
-- **Begin Haskell Syntax**
{
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE FlexibleContexts #-}

module Lexer (
  Token(..),
  IToken(..),
  AlexPosn(..),
  remDupEOLs,
  scanTokens
) where

import Syntax

import Control.Monad.Except
}
-- **End Haskell Syntax**

-- TODO use --latin1 option?
-- TODO use bytestrings?

-- **Begin Alex Syntax**
%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n\r]

tokens :-

  -- TODO figure out why we still get duplicate TokenEOL in output!
  -- Whitespace insensitive, but preserve newlines
  [\ \t\f\v]+                   ;

  -- Comments
  "'".*                         ;

  -- Line continuations
  " _" $eol+                    ;

  -- Syntax
  \"([^\"]+)\"                  { \p s -> Token p $ TokenStringLit (read s) }
  True                          { \p s -> Token p $ TokenTrue }
  False                         { \p s -> Token p $ TokenFalse }
  Attribute                     { \p s -> Token p $ TokenAttribute }
  Option                        { \p s -> Token p $ TokenOption }
  Double                        { \p s -> Token p $ TokenDouble }
  Integer                       { \p s -> Token p $ TokenInteger }
  Boolean                       { \p s -> Token p $ TokenBoolean }
  String                        { \p s -> Token p $ TokenString }
  Dim                           { \p s -> Token p $ TokenDim }
  ByVal                         { \p s -> Token p $ TokenByVal }
  ByRef                         { \p s -> Token p $ TokenByRef }
  Public                        { \p s -> Token p $ TokenPublic }
  Private                       { \p s -> Token p $ TokenPrivate }
  Function                      { \p s -> Token p $ TokenFunction }
  Sub                           { \p s -> Token p $ TokenSubroutine }
  With                          { \p s -> Token p $ TokenWith }
  Declare                       { \p s -> Token p $ TokenDeclare }
  Lib                           { \p s -> Token p $ TokenLib }
  Type                          { \p s -> Token p $ TokenType }
  For                           { \p s -> Token p $ TokenFor }
  To                            { \p s -> Token p $ TokenTo }
  Step                          { \p s -> Token p $ TokenStep }
  If                            { \p s -> Token p $ TokenIf }
  Then                          { \p s -> Token p $ TokenThen }
  Else                          { \p s -> Token p $ TokenElse }
  Or                            { \p s -> Token p $ TokenOr }
  And                           { \p s -> Token p $ TokenAnd }
  Next                          { \p s -> Token p $ TokenNext }
  End                           { \p s -> Token p $ TokenEnd }
  Exit                          { \p s -> Token p $ TokenExit }
  Explicit                      { \p s -> Token p $ TokenExplicit }
  As                            { \p s -> Token p $ TokenAs }
  $digit+                       { \p s -> Token p $ TokenIntLit (read s) }
  $digit+ \#                    { \p s -> Token p $ TokenDoubleLit ((read . reverse . (drop 1) . reverse) s) }
  $digit+ \. $digit+            { \p s -> Token p $ TokenDoubleLit (read s) }
  \=                            { \p s -> Token p $ TokenEq }
  \>                            { \p s -> Token p $ TokenGt }
  \>\=                          { \p s -> Token p $ TokenGeq }
  \<                            { \p s -> Token p $ TokenLt }
  \<\=                          { \p s -> Token p $ TokenLeq }
  \+                            { \p s -> Token p $ TokenAdd }
  \-                            { \p s -> Token p $ TokenSub }
  \*                            { \p s -> Token p $ TokenMul }
  \/                            { \p s -> Token p $ TokenDiv }
  \.                            { \p s -> Token p $ TokenDot }
  \(                            { \p s -> Token p $ TokenLParen }
  \)                            { \p s -> Token p $ TokenRParen }
  [\,]                          { \p s -> Token p $ TokenComma }
  $alpha [$alpha $digit \_]*    { \p s -> Token p $ TokenSym s }
  $eol+                         { \p s -> Token p $ TokenEOL }

-- **End Alex Syntax**

{
data Token = Token AlexPosn IToken
  deriving (Eq)

instance Show Token where
  show (Token _ t) = show t

data IToken
  = TokenTrue
  | TokenFalse
  | TokenAs
  | TokenDot
  | TokenFor | TokenTo | TokenStep | TokenNext
  | TokenIf | TokenThen | TokenElse
  | TokenOr | TokenAnd
  | TokenDouble
  | TokenInteger
  | TokenBoolean
  | TokenString
  | TokenOption
  | TokenDim | TokenByVal | TokenByRef
  | TokenFunction | TokenSubroutine
  | TokenWith
  | TokenType
  | TokenPublic
  | TokenDeclare | TokenLib
  | TokenPrivate
  | TokenEnd
  | TokenEndFunction
  | TokenEndType
  | TokenExit
  | TokenExplicit
  | TokenAttribute
  | TokenIntLit Int
  | TokenDoubleLit Double
  | TokenStringLit String
  | TokenEq
  | TokenAdd
  | TokenSub
  | TokenMul
  | TokenDiv
  | TokenGt | TokenGeq | TokenLt | TokenLeq
  | TokenLParen
  | TokenRParen
  | TokenComma
  | TokenSym String
  | TokenEOL
  | TokenEOF
  deriving (Eq, Show)

remDupEOLs :: [Token] -> [Token]
remDupEOLs []  = []
remDupEOLs [x] = [x]
remDupEOLs (first@(Token _ TokenEOL):(Token _ TokenEOL):xs) = remDupEOLs (first : xs)
remDupEOLs (x1 : xs) = x1 : (remDupEOLs xs)

scanTokens :: String -> Except String [Token]
scanTokens str = go (alexStartPos, '\n', [], str) where
  go inp@(pos, _, _bs, rest_str) =
    case alexScan inp 0 of
      AlexEOF -> return []
      AlexError ((AlexPn _ line column), prevChar, restOfBytes, _) ->
        throwError $ "Lexical error on line "
          ++ (show line) ++ ", column "
          ++ (show column) ++ "." ++ "\n"
          ++ (lines str !! (line - 1)) ++ "\n"
          ++ (take (column - 1) (repeat '-')) ++ "^\n"
      AlexSkip  inp' len     -> go inp'
      AlexToken inp' len act -> do
        res <- go inp'
        let rest = act pos (take len rest_str)
        return (rest : res)
}
