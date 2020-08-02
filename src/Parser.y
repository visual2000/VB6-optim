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

import Prelude hiding (LT, GT)
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
    'Declare'     { Token _ (TokenDeclare) }
    'Lib'         { Token _ (TokenLib) }
    'LSet'        { Token _ (TokenLSet) }
    'Set'         { Token _ (TokenSet) }
    'Function'    { Token _ (TokenFunction) }
    'Sub'         { Token _ (TokenSubroutine) }
    'ByRef'       { Token _ (TokenByRef) }
    'ByVal'       { Token _ (TokenByVal) }
    'Type'        { Token _ (TokenType) }
    'With'        { Token _ (TokenWith) }
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
    INT           { Token _ (TokenIntLit $$) }
    DOUBLE        { Token _ (TokenDoubleLit $$) }
    VAR           { Token _ (TokenSym $$) }
    STR           { Token _ (TokenStringLit $$) }
    'Attribute'   { Token _ (TokenAttribute) }
    'Option'      { Token _ (TokenOption) }
    'Dim'         { Token _ (TokenDim) }
    'Explicit'    { Token _ (TokenExplicit) }
    'As'          { Token _ (TokenAs) }
    'Do'          { Token _ (TokenDo) }
    'Loop'        { Token _ (TokenLoop) }
    'While'       { Token _ (TokenWhile) }
    '='           { Token _ (TokenEq) }
    '+'           { Token _ (TokenAdd) }
    '-'           { Token _ (TokenSub) }
    '*'           { Token _ (TokenMul) }
    '/'           { Token _ (TokenDiv) }
    '>'           { Token _ (TokenGt) }
    '<'           { Token _ (TokenLt) }
    '>='          { Token _ (TokenGeq) }
    '<='          { Token _ (TokenLeq) }
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
%right ','
%left 'And' 'Or'
%left '<' '>' '=' '<=' '>='
%left '+' '-'
%left '*' '/'
%right '.'
%%

Module : ModuleAttributes
         ModuleOptions
         TopLevelDeclarations
         { Mod $1 $2 $3 }

TopLevelDeclarations : {- empty -}        { [] }
                     | TopLevelDeclaration TopLevelDeclarations { $1 : $2 }

TopLevelDeclaration : Visibility 'Type' VAR eol
                                     UserTypeDeclFields
                                 'End' 'Type' eol      { UserTypeDecl $1 $3 $5 }
                    | Visibility 'Declare' 'Function'
                         VAR 'Lib'
                         STR '(' FnDeclArgs ')' 'As' TypeRef eol
                                                       { DllFunc $1 $4 $6 $8 $11 }
                    | Visibility 'Function' VAR '(' FnDeclArgs ')' 'As' TypeRef eol
                                     Statements
                                 'End' 'Function' eol      { FuncDecl $1 $3 $5 $8 $10 }
                    | Visibility 'Sub' VAR '(' FnDeclArgs ')' eol
                                     Statements
                                 'End' 'Sub' eol      { SubDecl $1 $3 $5 $8 }
                    | Visibility VAR 'As' TypeRef eol  { GlobalVarDecl $1 $2 $4 }

Visibility : 'Private' { Private }
           | 'Public'  { Public }

UserTypeDeclFields : UserTypeDeclField                 { [$1] }
              | UserTypeDeclField UserTypeDeclFields   { $1 : $2 }

UserTypeDeclField : DimDeclArg eol { $1 }

ModuleAttributes : {- empty -}          { [] }
           | ModuleAttribute ModuleAttributes { $1 : $2 }

ModuleAttribute : 'Attribute' VAR '=' Lit eol { ModuleAttribute $2 $4 }

ModuleOptions : {- empty -}    { [] }
        | ModuleOption ModuleOptions { $1 : $2 }

ModuleOption : 'Option' 'Explicit' eol { OptionExplicit }

TypeRef : 'Double'  { TDouble }
        | 'Integer' { TInt }
        | 'String'  { TString }
        | 'Boolean' { TBoolean }
        | VAR       { TUDT $1 }

FnDeclArgs : {- empty -}               { [] }
           | FnDeclArg                 { [$1] } -- TODO disallow trailing ','
           | FnDeclArg ',' FnDeclArgs  { $1 : $3 }

FnDeclArg : VAR 'As' TypeRef             { Unspecified $ TypeDecl $1 $3 }
          | VAR '(' ')' 'As' TypeRef     { Unspecified $ TypeDeclArray $1 $5 }
          | 'ByRef' VAR 'As' TypeRef             { ByRef $ TypeDecl $2 $4 }
          | 'ByRef' VAR '(' ')' 'As' TypeRef     { ByRef $ TypeDeclArray $2 $6 }
          | 'ByVal' VAR 'As' TypeRef             { ByVal $ TypeDecl $2 $4 }
          | 'ByVal' VAR '(' ')' 'As' TypeRef     { ByVal $ TypeDeclArray $2 $6 }

DimDeclArgs : {- empty -}               { [] }
            | DimDeclArg                 { [$1] } -- TODO disallow trailing ','
            | DimDeclArg ',' DimDeclArgs  { $1 : $3 }

DimDeclArg : VAR 'As' TypeRef            { TypeDecl $1 $3 }
           | VAR '(' ')' 'As' TypeRef    { TypeDeclArray $1 $5 }
           | VAR '(' INT ')' 'As' TypeRef
                                         { TypeDeclArrayWithUpperBound $1 $3 $6 }
           | VAR '(' INT 'To' INT ')' 'As' TypeRef
                                         { TypeDeclArrayWithBounds $1 $3 $5 $8 }

Statements : {- empty -}              { [] }
           | Statement Statements     { $1 : $2 }

Statement : 'Dim' DimDeclArgs eol         { StmtDecl $2 }
          | 'LSet' Lhs '=' Expr eol       { StmtLSetAssign $2 $4 }
          | 'Set' Lhs '=' Expr eol        { StmtSetAssign $2 $4 }
          | Lhs '=' Expr eol              { StmtAssign $1 $3 }
          | 'If' Expr 'Then' eol
                Statements
            'Else' eol
                Statements
            'End' 'If' eol                { StmtIfThenElse $2 $5 $8 }
          | 'With' Lhs eol
                Withs
            'End' 'With' eol              { StmtWith $2 $4 }
          | 'If' Expr 'Then' eol
                Statements
            'End' 'If' eol                { StmtIfThenElse $2 $5 [] }
          | 'For' VAR '=' Expr 'To' Expr 'Step' Expr eol
                Statements
            'Next' VAR eol                { StmtFor $2 $4 $6 (Just $8) $10 }
          | 'For' VAR '=' Expr 'To' Expr eol
                Statements
            'Next' VAR eol                { StmtFor $2 $4 $6 Nothing $8 }
          | 'Exit' 'Function' eol         { StmtReturn }
          -- FIXME FNCallRef rule is causing a shift/reduce conflict.
          | FNCallRef ExprList eol        { StmtNakedFunctionCall $1 $2 }
          | 'Do' eol
                Statements
            'Loop' 'While' Expr eol       { StmtDoStatementsLoopWhileCond $3 $6 }

Withs : With { [$1] }
      | With Withs { $1 : $2 }

With : '.' Lhs '=' Expr eol { WithAssignment $2 $4 }

Lhs : VAR             { NameLhs $1 }
    | VAR '.' Lhs     { FieldLhs $1 $3 }
    | VAR '(' INT ')' { ArrayLhs $1 $3 }

FNCallRef : VAR                { NameLhs $1 }
          | VAR '.' FNCallRef  { FieldLhs $1 $3 }

Expr : FNCallRef '(' ExprList ')' { ECall $1 $3 }
     | '-' Expr                   { ENeg $2 }
     | Expr 'And' Expr            { EOp And $1 $3 }
     | Expr 'Or' Expr             { EOp Or $1 $3 }
     | Expr '<' Expr              { EOp LT $1 $3 }
     | Expr '>' Expr              { EOp GT $1 $3 }
     | Expr '<=' Expr             { EOp LEQ $1 $3 }
     | Expr '>=' Expr             { EOp GEQ $1 $3 }
     | Expr '+' Expr              { EOp Add $1 $3 }
     | Expr '-' Expr              { EOp Sub $1 $3 }
     | Expr '*' Expr              { EOp Mul $1 $3 }
     | Expr '/' Expr              { EOp Div $1 $3 }
     | '(' Expr ')'               { $2 }
     | Lit                        { ELit $1 }
     | VariableAccess             { EAccess $1 }

VariableAccess : VAR                    { [ $1 ] }
               | VAR '.' VariableAccess { $1 : $3 }

ExprList : {-empty -}         { [] }
         | NonEmptyExprList   { $1 }

NonEmptyExprList : Expr                      { [$1] }
                 | Expr ',' NonEmptyExprList { $1 : $3 }

Lit  : INT                         { LInt $1 }
     | 'True'                      { LBool True }
     | 'False'                     { LBool False }
     | STR                         { LString $1 }
     | DOUBLE                      { LDouble $1 }

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
parseTokens = runExcept . lexStringToTokens

}
