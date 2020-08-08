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
  lexStringToTokens
) where

import AG

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
  And                           { \p s -> Token p $ TokenAnd }
  As                            { \p s -> Token p $ TokenAs }
  Attribute                     { \p s -> Token p $ TokenAttribute }
  Boolean                       { \p s -> Token p $ TokenBoolean }
  ByRef                         { \p s -> Token p $ TokenByRef }
  ByVal                         { \p s -> Token p $ TokenByVal }
  Call                          { \p s -> Token p $ TokenCall }
  Declare                       { \p s -> Token p $ TokenDeclare }
  Dim                           { \p s -> Token p $ TokenDim }
  Do                            { \p s -> Token p $ TokenDo }
  Double                        { \p s -> Token p $ TokenDouble }
  Else                          { \p s -> Token p $ TokenElse }
  End                           { \p s -> Token p $ TokenEnd }
  Exit                          { \p s -> Token p $ TokenExit }
  Explicit                      { \p s -> Token p $ TokenExplicit }
  False                         { \p s -> Token p $ TokenFalse }
  For                           { \p s -> Token p $ TokenFor }
  Function                      { \p s -> Token p $ TokenFunction }
  If                            { \p s -> Token p $ TokenIf }
  Integer                       { \p s -> Token p $ TokenInteger }
  LSet                          { \p s -> Token p $ TokenLSet }
  Lib                           { \p s -> Token p $ TokenLib }
  Loop                          { \p s -> Token p $ TokenLoop }
  Next                          { \p s -> Token p $ TokenNext }
  Option                        { \p s -> Token p $ TokenOption }
  Or                            { \p s -> Token p $ TokenOr }
  Private                       { \p s -> Token p $ TokenPrivate }
  Public                        { \p s -> Token p $ TokenPublic }
  Set                           { \p s -> Token p $ TokenSet }
  Step                          { \p s -> Token p $ TokenStep }
  String                        { \p s -> Token p $ TokenString }
  Sub                           { \p s -> Token p $ TokenSubroutine }
  Then                          { \p s -> Token p $ TokenThen }
  To                            { \p s -> Token p $ TokenTo }
  True                          { \p s -> Token p $ TokenTrue }
  Type                          { \p s -> Token p $ TokenType }
  While                         { \p s -> Token p $ TokenWhile }
  With                          { \p s -> Token p $ TokenWith }
  $digit+                       { \p s -> Token p $ TokenIntLit (read s) }
  $digit+ \#                    { \p s -> Token p $ TokenDoubleLit ((read . reverse . (drop 1) . reverse) s) }
  $digit+ \. $digit+            { \p s -> Token p $ TokenDoubleLit (read s) }
  \(                            { \p s -> Token p $ TokenLParen }
  \)                            { \p s -> Token p $ TokenRParen }
  \*                            { \p s -> Token p $ TokenMul }
  \+                            { \p s -> Token p $ TokenAdd }
  \-                            { \p s -> Token p $ TokenSub }
  \.                            { \p s -> Token p $ TokenDot }
  \/                            { \p s -> Token p $ TokenDiv }
  \<                            { \p s -> Token p $ TokenLt }
  \<\=                          { \p s -> Token p $ TokenLeq }
  \=                            { \p s -> Token p $ TokenEq }
  \>                            { \p s -> Token p $ TokenGt }
  \>\=                          { \p s -> Token p $ TokenGeq }
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
  = TokenAdd
  | TokenAs
  | TokenAttribute
  | TokenBoolean
  | TokenCall
  | TokenComma
  | TokenDeclare | TokenLib
  | TokenDim | TokenByVal | TokenByRef
  | TokenDiv
  | TokenDo
  | TokenDot
  | TokenDouble
  | TokenDoubleLit Double
  | TokenEOF
  | TokenEOL
  | TokenEnd
  | TokenEndFunction
  | TokenEndType
  | TokenEq
  | TokenExit
  | TokenExplicit
  | TokenFalse
  | TokenFor | TokenTo | TokenStep | TokenNext
  | TokenFunction | TokenSubroutine
  | TokenGt | TokenGeq | TokenLt | TokenLeq
  | TokenIf | TokenThen | TokenElse
  | TokenIntLit Int
  | TokenInteger
  | TokenLParen | TokenRParen
  | TokenLoop
  | TokenMul
  | TokenOption
  | TokenOr | TokenAnd
  | TokenPrivate
  | TokenPublic
  | TokenSet | TokenLSet
  | TokenString
  | TokenStringLit String
  | TokenSub
  | TokenSym String
  | TokenTrue
  | TokenType
  | TokenWhile
  | TokenWith
  deriving (Eq, Show)

remDupEOLs :: [Token] -> [Token]
remDupEOLs []  = []
remDupEOLs [x] = [x]
remDupEOLs (first@(Token _ TokenEOL):(Token _ TokenEOL):xs) = remDupEOLs (first : xs)
remDupEOLs (x1 : xs) = x1 : (remDupEOLs xs)

lexStringToTokens :: String -> Except String [Token]
lexStringToTokens str = go (alexStartPos, '\n', [], str) where
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
