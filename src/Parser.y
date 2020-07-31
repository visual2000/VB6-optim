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
    'Function'    { Token _ (TokenFunction) }
    'Type'        { Token _ (TokenType) }
    'End'         { Token _ (TokenEnd) }
    'For'         { Token _ (TokenFor) }
    'To'          { Token _ (TokenTo) }
    'Next'        { Token _ (TokenNext) }
    'Step'        { Token _ (TokenStep) }
    'Or'          { Token _ (TokenOr) }
    'And'         { Token _ (TokenAnd) }
    'If'          { Token _ (TokenIf) }
    'Then'        { Token _ (TokenThen) }
    'Else'        { Token _ (TokenElse) }
    'Exit'        { Token _ (TokenExit) }
    'Public'      { Token _ (TokenPublic) }
    'Private'     { Token _ (TokenPrivate) }
    'True'        { Token _ (TokenTrue) }
    'False'       { Token _ (TokenFalse) }
    NUM           { Token _ (TokenNum $$) }
    VAR           { Token _ (TokenSym $$) }
    STR           { Token _ (TokenStringLit $$) }
    'Attribute'   { Token _ (TokenAttribute) }
    'Option'      { Token _ (TokenOption) }
    'Dim'         { Token _ (TokenDim) }
    'Explicit'    { Token _ (TokenExplicit) }
    'As'          { Token _ (TokenAs) }
    '='           { Token _ (TokenEq) }
    '+'           { Token _ (TokenAdd) }
    '-'           { Token _ (TokenSub) }
    '*'           { Token _ (TokenMul) }
    '.'           { Token _ (TokenDot) }
    'Double'      { Token _ (TokenDouble) }
    'Integer'     { Token _ (TokenInteger) }
    'Boolean'     { Token _ (TokenBoolean) }
    'String'      { Token _ (TokenString) }
    '('           { Token _ (TokenLParen) }
    ')'           { Token _ (TokenRParen) }
    ','           { Token _ (TokenComma ) }
    eol           { Token _ (TokenEOL) }

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

TopLevelDeclaration : Visibility 'Type' VAR eol
                                     TypeDefFields
                                 'End' 'Type' eol      { Left (TypeDef $1 $3 $5) }
                    | Visibility 'Function' VAR '(' FnDeclArgs ')' 'As' TypeRef eol
                                     Statements
                                 'End' 'Function' eol      { Right (FuncDecl $1 $3 $5 $8 $10) }

Visibility : 'Private' { Private }
           | 'Public'  { Public }

TypeDefFields : TypeDefField                 { [$1] }
              | TypeDefField TypeDefFields   { $1 : $2 }

TypeDefField : VAR 'As' TypeRef eol { TypeField $1 $3 }

Attributes : {- empty -}          { [] }
           | Attribute Attributes { $1 : $2 }

Attribute : 'Attribute' VAR '=' Lit eol { Attribute $2 $4 }

Options : {- empty -}    { [] }
        | Option Options { $1 : $2 }

Option : 'Option' 'Explicit' eol { OptionExplicit }

TypeRef : 'Double'  { TDouble }
        | 'Integer' { TInt }
        | 'String'  { TString }
        | 'Boolean' { TBoolean }
        | VAR       { TUDT $1 }

FnDeclArgs : {- empty -}               { [] }
           | FnDeclArg                 { [$1] } -- TODO disallow trailing ','
           | FnDeclArg ',' FnDeclArgs  { $1 : $3 }

FnDeclArg : VAR 'As' TypeRef             { TypeField $1 $3 }
          | VAR '(' ')' 'As' TypeRef     { TypeFieldArray $1 $5 }

Statements : {- empty -}              { [] }
           | Statement Statements     { $1 : $2 }

Statement : 'Dim' FnDeclArgs eol          { StmtDecl $2 }
          | Lhs '=' Expr eol              { StmtAssign $1 $3 }
          | 'If' Expr 'Then' eol
                Statements
            'End' 'If' eol                { StmtIfThenElse $2 $5 [] }
          | 'For' VAR '=' Expr 'To' Expr eol
                Statements
            'Next' VAR eol                { StmtFor $2 $4 $6 1 $8 }

Lhs : VAR             { NameLhs $1 }
    | VAR '.' VAR     { FieldLhs [$1, $3] } -- for now only single dot
    | VAR '(' NUM ')' { ArrayLhs $1 $3 }

FNCallRef : VAR             { NameLhs $1 }
          | VAR '.' VAR     { FieldLhs [$1, $3] } -- for now only single dot

Expr : FNCallRef '(' ExprList ')' { ECall $1 $3 }
     | Lit                        { ELit $1 }
     | VAR                        { EVar $1 }
     | VAR '.' VAR                { EAccess [$1, $3] }
     | Expr '+' Expr              { EOp Add $1 $3 }
     | Expr '-' Expr              { EOp Sub $1 $3 }
     | Expr '*' Expr              { EOp Mul $1 $3 }
--      | '(' Expr ')'             { $2 }

ExprList : Expr              { [$1] }
         | Expr ',' ExprList { $1 : $3 }

Lit  : NUM                         { LInt $1 }
     | 'True'                      { LBool True }
     | 'False'                     { LBool False }
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
  modu (remDupEOLs input)

-- todo rename to lexTokens
parseTokens :: String -> Either String [Token]
parseTokens = runExcept . scanTokens

}
