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
import Data.Either

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
    func        { TokenFunction }
    type        { TokenType }
    end         { TokenEnd }
    public      { TokenPublic }
    private     { TokenPrivate }
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
         TopLevelDeclarations
         { Mod $1 $2 (lefts $3) (rights $3) }

TopLevelDeclarations : {- empty -}        { [] }
                     | TopLevelDeclaration TopLevelDeclarations { $1 : $2 }

TopLevelDeclaration : Visibility type VAR eol
                                     TypeDefFields
                                 end type eol      { Left (TypeDef $1 $3 $5) }
                    | Visibility func VAR '(' FnDeclArgs ')' as TypeRef eol
                                     Statements
                                 end func eol      { Right (FuncDecl $1 $3 $5 $8 $10) }

Visibility : private { Private }
           | public  { Public }

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
