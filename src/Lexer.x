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
%wrapper "basic"

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
  True                          { \s -> TokenTrue }
  False                         { \s -> TokenFalse }
  Attribute                     { \s -> TokenAttribute }
  Option                        { \s -> TokenOption }
  Explicit                      { \s -> TokenExplicit }
  $digit+                       { \s -> TokenNum (read s) }
  \"([^\"]+)\"                  { \s -> TokenString (read s) }
  \=                            { \s -> TokenEq }
  [\+]                          { \s -> TokenAdd }
  [\-]                          { \s -> TokenSub }
  [\*]                          { \s -> TokenMul }
  \(                            { \s -> TokenLParen }
  \)                            { \s -> TokenRParen }
  $alpha [$alpha $digit \_]*    { \s -> TokenSym s }
  $eol+                         { \s -> TokenEOL }

-- **End Alex Syntax**

{
data Token
  = TokenTrue
  | TokenFalse
  | TokenOption
  | TokenExplicit
  | TokenAttribute
  | TokenNum Int
  | TokenString String
  | TokenEq
  | TokenAdd
  | TokenSub
  | TokenMul
  | TokenLParen
  | TokenRParen
  | TokenSym String
  | TokenEOL
  | TokenEOF
  deriving (Eq,Show)

scanTokens :: String -> Except String [Token]
scanTokens str = go ('\n',[],str) where
  go inp@(_,_bs,str) =
    case alexScan inp 0 of
     AlexEOF -> return []
     AlexError _ -> throwError "Invalid lexeme."
     AlexSkip  inp' len     -> go inp'
     AlexToken inp' len act -> do
      res <- go inp'
      let rest = act (take len str)
      return (rest : res)
}
