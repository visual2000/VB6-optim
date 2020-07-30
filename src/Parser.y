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
    pub_func    { TokenPublicFunction }
    pub_type    { TokenPublicType }
    end_func    { TokenEndFunction }
    end_type    { TokenEndType }
    true        { TokenTrue }
    false       { TokenFalse }
    NUM         { TokenNum $$ }
    VAR         { TokenSym $$ }
    STR         { TokenStringLit $$ }
    attr        { TokenAttribute }
    opt         { TokenOption }
    dim         { TokenDim }
    explicit    { TokenExplicit }
    as          { TokenAs }
    '='         { TokenEq }
    '+'         { TokenAdd }
    '-'         { TokenSub }
    '*'         { TokenMul }
    '.'         { TokenDot }
    'Double'    { TokenDouble }
    'Integer'   { TokenInteger }
    'Boolean'   { TokenBoolean }
    'String'    { TokenString }
    '('         { TokenLParen }
    ')'         { TokenRParen }
    ','         { TokenComma  }
    eol         { TokenEOL }

-- Operators
%left '+' '-'
%left '*' '.'
%%

Module : Attributes
         Options
         TypeDefs
         FuncDecls
         { Mod $1 $2 $3 $4 }

TypeDefs : {- empty-}       { [] }
         | TypeDef TypeDefs { $1 : $2 }

TypeDef : pub_type VAR eol
              TypeDefFields
          end_type eol              { TypeDef Public $2 $4 }

TypeDefFields : TypeDefField                 { [$1] }
              | TypeDefField TypeDefFields   { $1 : $2 }

TypeDefField : VAR as TypeRef eol { TypeField $1 $3 }

Attributes : {- empty -}       { [] }
           | Attribute Attributes { $1 : $2 }

Attribute : attr VAR '=' Lit eol { Attribute $2 $4 }

Options : {- empty -}    { [] }
        | Option Options { $1 : $2 }

Option : opt explicit eol { OptionExplicit }

TypeRef : 'Double'  { TDouble }
        | 'Integer' { TInt }
        | 'String'  { TString }
        | VAR       { TUDT $1 }

FuncDecls : {- empty -}        { [] }
          | FuncDecl FuncDecls { $1 : $2 }

FuncDecl : pub_func VAR '(' FnDeclArgs ')' as TypeRef eol
           Statements
           end_func eol                  { FuncDecl Public $2 $4 $7 $9 }

FnDeclArgs : {- empty -}               { [] }
           | FnDeclArg                 { [$1] } -- TODO disallow trailing ','
           | FnDeclArg ',' FnDeclArgs  { $1 : $3 }

FnDeclArg : VAR as TypeRef             { TypeField $1 $3 }

Statements : {- empty -}              { [] }
           | Statement Statements     { $1 : $2 }

Statement : dim VAR as TypeRef eol    { StmtDecl $2 $4 }
          | Lhs '=' Expr eol          { StmtAssign $1 $3 }

Lhs : VAR             { NameLhs $1 }
    | VAR '.' VAR     { FieldLhs [$1, $3] } -- for now only single dot
    | VAR '(' NUM ')' { ArrayLhs $1 $3 }

Expr : Lit                { ELit $1 }
     | VAR                { EVar $1 }
     | Form               { $1 }

Form : Form '+' Form               { EOp Add $1 $3 }
     | Form '-' Form               { EOp Sub $1 $3 }
     | Form '*' Form               { EOp Mul $1 $3 }
     | Atom                        { $1 }

Atom : '(' Expr ')'                { $2 }

Lit  : NUM                         { LInt $1 }
     | true                        { LBool True }
     | false                       { LBool False }
     | STR                         { LString $1 }

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
