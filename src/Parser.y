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
    explicit { TokenExplicit }
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

Module : Attributes Options { Mod $1 $2 ([]) }

Attributes : Attribute         { [$1] }
           | Attributes Attribute { $2 : $1 }

Attribute : attr VAR '=' Atom eol { Attribute $2 $4 }

Options : Option { [$1] }
        | Options Option { $2 : $1 }

Option : opt explicit eol { OptionExplicit }

-- Form : Form '+' Form               { Op Add $1 $3 }
--      | Form '-' Form               { Op Sub $1 $3 }
--      | Form '*' Form               { Op Mul $1 $3 }
--      | Atom                        { $1 }

-- Atom : '(' Expr ')'                { $2 }
Atom : NUM                         { (LInt $1) }
  --    | VAR                         { Var $1 }
     | STR                         { (LString $1) }
     | true                        { (LBool True) }
     | false                       { (LBool False) }

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
