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
%monad { Except (String -> String) } { (>>=) } { return }
%error { parseError }

-- Token Names
%token
    func        { Token _ (TokenFunction) }
    type        { Token _ (TokenType) }
    end         { Token _ (TokenEnd) }
    public      { Token _ (TokenPublic) }
    private     { Token _ (TokenPrivate) }
    true        { Token _ (TokenTrue) }
    false       { Token _ (TokenFalse) }
    NUM         { Token _ (TokenNum $$) }
    VAR         { Token _ (TokenSym $$) }
    STR         { Token _ (TokenStringLit $$) }
    attr        { Token _ (TokenAttribute) }
    opt         { Token _ (TokenOption) }
    dim         { Token _ (TokenDim) }
    explicit    { Token _ (TokenExplicit) }
    as          { Token _ (TokenAs) }
    '='         { Token _ (TokenEq) }
    '+'         { Token _ (TokenAdd) }
    '-'         { Token _ (TokenSub) }
    '*'         { Token _ (TokenMul) }
    '.'         { Token _ (TokenDot) }
    'Double'    { Token _ (TokenDouble) }
    'Integer'   { Token _ (TokenInteger) }
    'Boolean'   { Token _ (TokenBoolean) }
    'String'    { Token _ (TokenString) }
    '('         { Token _ (TokenLParen) }
    ')'         { Token _ (TokenRParen) }
    ','         { Token _ (TokenComma ) }
    eol         { Token _ (TokenEOL) }

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
          | VAR '(' ')' as TypeRef     { TypeField $1 (TArrayOf $5) }

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

showTokenError :: Token -> String -> String
showTokenError (Token (AlexPn _ line column) t) orig_input =
               "Unexpected token " ++ show t ++ " on line "
                               ++ (show line) ++ ", column "
                               ++ (show column) ++ "." ++ "\n"
                               ++ (lines orig_input !! (line - 1)) ++ "\n"
                               ++ (take (column - 1) (repeat '-')) ++ "^\n"

parseError :: [Token] -> Except (String -> String) a
parseError (l:ls) = throwError $ (showTokenError l)
parseError [] = throwError (\_ -> "Unexpected: end of file")

parseModule :: [Token] -> Either (String -> String) Module
parseModule input = runExcept $ do
  modu input

-- todo rename to lexTokens
parseTokens :: String -> Either String [Token]
parseTokens = runExcept . scanTokens

}
