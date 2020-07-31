-- -*- mode: prog -*-
-- **Begin Haskell Syntax**
{
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE FlexibleContexts #-}

module Lexer (
  Token(..),
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
$eol   = [\n]

tokens :-

  -- Whitespace insensitive, but preserve newlines
  [\ \t\f\v]+                   ;

  -- Comments
  "'".*                         ;

  -- Line continuations
  " _" $eol                     ;

  -- Syntax
  \"([^\"]+)\"                  { \_ s -> TokenStringLit (read s) }
  True                          { \_ s -> TokenTrue }
  False                         { \_ s -> TokenFalse }
  Attribute                     { \_ s -> TokenAttribute }
  Option                        { \_ s -> TokenOption }
  Double                        { \_ s -> TokenDouble }
  Integer                       { \_ s -> TokenInteger }
  Boolean                       { \_ s -> TokenBoolean }
  String                        { \_ s -> TokenString }
  Dim                           { \_ s -> TokenDim }
  "Public Function"             { \_ s -> TokenPublicFunction }
  "Public Type"                 { \_ s -> TokenPublicType }
  "End Function"                { \_ s -> TokenEndFunction }
  "End Type"                    { \_ s -> TokenEndType }
  Explicit                      { \_ s -> TokenExplicit }
  As                            { \_ s -> TokenAs }
  $digit+                       { \_ s -> TokenNum (read s) }
  \=                            { \_ s -> TokenEq }
  [\+]                          { \_ s -> TokenAdd }
  [\-]                          { \_ s -> TokenSub }
  [\*]                          { \_ s -> TokenMul }
  [\.]                          { \_ s -> TokenDot }
  \(                            { \_ s -> TokenLParen }
  \)                            { \_ s -> TokenRParen }
  [\,]                          { \_ s -> TokenComma }
  $alpha [$alpha $digit \_]*    { \_ s -> TokenSym s }
  $eol+                         { \_ s -> TokenEOL }

-- **End Alex Syntax**

{
data Token
  = TokenTrue
  | TokenFalse
  | TokenAs
  | TokenDot
  | TokenDouble
  | TokenInteger
  | TokenBoolean
  | TokenString
  | TokenOption
  | TokenDim
  | TokenPublicFunction
  | TokenPublicType
  | TokenEndFunction
  | TokenEndType
  | TokenExplicit
  | TokenAttribute
  | TokenNum Int
  | TokenStringLit String
  | TokenEq
  | TokenAdd
  | TokenSub
  | TokenMul
  | TokenLParen
  | TokenRParen
  | TokenComma
  | TokenSym String
  | TokenEOL
  | TokenEOF
  deriving (Eq,Show)

scanTokens :: String -> Except String [Token]
scanTokens str = go (alexStartPos, '\n', [], str) where
  go inp@(pos, _, _bs, str) =
    case alexScan inp 0 of
      AlexEOF -> return []
      AlexError ((AlexPn _ line column),_,_,_) -> throwError $ "lexical error at " ++ (show line) ++ " line, " ++ (show column) ++ " column"
      AlexSkip  inp' len     -> go inp'
      AlexToken inp' len act -> do
        res <- go inp'
        let rest = act pos (take len str)
        return (rest : res)
}
