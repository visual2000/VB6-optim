-- -*- mode: prog -*-
{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser (
  parseModule,
  parseTokens,
) where

import Lexer
import Syntax

import Control.Monad.Except

}

-- Entry point
%name modu

-- Lexer structure
%tokentype { Token }

-- Parser monad
%monad { Except String } { (>>=) } { return }
%error { parseError }

-- Token Names
%token
    true  { TokenTrue }
    false { TokenFalse }
    NUM   { TokenNum $$ }
    VAR   { TokenSym $$ }
    STR   { TokenString $$ }
    attr  { TokenAttribute }
    opt   { TokenOption }
    pub   { TokenPublic }
    priv  { TokenPrivate }
    explicit { TokenExplicit }
    end   { TokenEnd }
    as    { TokenAs }
    type_ { TokenType }
    '='   { TokenEq }
    '+'   { TokenAdd }
    '-'   { TokenSub }
    '*'   { TokenMul }
    '('   { TokenLParen }
    ')'   { TokenRParen }
    eol   { TokenEOL }

-- Operators
%left '+' '-'
%left '*'
%%

Module : Attributes Options TypeDefs { Mod $1 $2 $3 [] }

TypeDefs : {- empty-}       { [] }
         | TypeDef TypeDefs { $1 : $2 }

TypeDef : Visibility type_ VAR eol
          TypeDefFields
          end type_ eol                      { TypeDef $1 $3 $5 }

TypeDefFields : TypeDefField                 { [$1] }
              | TypeDefField TypeDefFields   { $1 : $2 }

TypeDefField : VAR as VAR eol { TypeField $1 (TUDT $3) }

Attributes : {- empty -}       { [] }
           | Attribute Attributes { $1 : $2 }

Attribute : attr VAR '=' Atom eol { Attribute $2 $4 }

Visibility : pub   { Public }
           | priv  { Private }

Options : {- empty -}    { [] }
        | Option Options { $1 : $2 }

Option : opt explicit eol { OptionExplicit }

-- Form : Form '+' Form               { Op Add $1 $3 }
--      | Form '-' Form               { Op Sub $1 $3 }
--      | Form '*' Form               { Op Mul $1 $3 }
--      | Atom                        { $1 }

-- Atom : '(' Expr ')'                { $2 }
Atom : NUM                         { LInt $1 }
  --    | VAR                         { Var $1 }
     | STR                         { LString $1 }
     | true                        { LBool True }
     | false                       { LBool False }

{

parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError [] = throwError "Unexpected end of Input"

parseModule :: String -> Either String Module
parseModule input = runExcept $ do
  tokenStream <- scanTokens input
  modu tokenStream

parseTokens :: String -> Either String [Token]
parseTokens = runExcept . scanTokens

}
