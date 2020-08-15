-- -*- mode: prog -*-
{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser (
  parseModule,
  parseTokens,
  rawParseModule,
  rawParseStatement,
) where

import Lexer
import AG

import Control.Monad.Except
import Data.Either

}

-- Entry points
%name rawParseModule            Module
%name rawParseStatement         Statement

-- Lexer structure
%tokentype { Token }

-- Parser monad
%monad { Except (String -> String) } { (>>=) } { return }
%error { parseError }

-- Token Names
%token
    '('           { Token _ (TokenLParen) }
    ')'           { Token _ (TokenRParen) }
    '*'           { Token _ (TokenMul) }
    '+'           { Token _ (TokenAdd) }
    ','           { Token _ (TokenComma ) }
    '-'           { Token _ (TokenSub) }
    '.'           { Token _ (TokenDot) }
    ':'           { Token _ (TokenColon) }
    '/'           { Token _ (TokenDiv) }
    '<'           { Token _ (TokenLt) }
    '<='          { Token _ (TokenLeq) }
    '='           { Token _ (TokenEq) }
    '>'           { Token _ (TokenGt) }
    '>='          { Token _ (TokenGeq) }
    'And'         { Token _ (TokenAnd) }
    'As'          { Token _ (TokenAs) }
    'Attribute'   { Token _ (TokenAttribute) }
    'Boolean'     { Token _ (TokenBoolean) }
    'ByRef'       { Token _ (TokenByRef) }
    'ByVal'       { Token _ (TokenByVal) }
    'Call'        { Token _ (TokenCall) }
    'Declare'     { Token _ (TokenDeclare) }
    'Dim'         { Token _ (TokenDim) }
    'Do'          { Token _ (TokenDo) }
    'Double'      { Token _ (TokenDouble) }
    'Else'        { Token _ (TokenElse) }
    'End'         { Token _ (TokenEnd) }
    'Exit'        { Token _ (TokenExit) }
    'Explicit'    { Token _ (TokenExplicit) }
    'False'       { Token _ (TokenFalse) }
    'For'         { Token _ (TokenFor) }
    'Function'    { Token _ (TokenFunction) }
    'GoTo'        { Token _ (TokenGoTo) }
    'If'          { Token _ (TokenIf) }
    'Integer'     { Token _ (TokenInteger) }
    'LSet'        { Token _ (TokenLSet) }
    'Lib'         { Token _ (TokenLib) }
    'Loop'        { Token _ (TokenLoop) }
    'Next'        { Token _ (TokenNext) }
    'Option'      { Token _ (TokenOption) }
    'Or'          { Token _ (TokenOr) }
    'Private'     { Token _ (TokenPrivate) }
    'Public'      { Token _ (TokenPublic) }
    'Set'         { Token _ (TokenSet) }
    'Step'        { Token _ (TokenStep) }
    'String'      { Token _ (TokenString) }
    'Sub'         { Token _ (TokenSubroutine) }
    'Then'        { Token _ (TokenThen) }
    'To'          { Token _ (TokenTo) }
    'True'        { Token _ (TokenTrue) }
    'Type'        { Token _ (TokenType) }
    'While'       { Token _ (TokenWhile) }
    DOUBLE        { Token _ (TokenDoubleLit $$) }
    INT           { Token _ (TokenIntLit $$) }
    STR           { Token _ (TokenStringLit $$) }
    VAR           { Token _ (TokenSym $$) }
    eol           { Token _ (TokenEOL) }

-- Operators
%right ','
%left 'And' 'Or'
%left '<' '>' '=' '<=' '>='
%left '+' '-'
%left '*' '/'
%nonassoc precUnaryMinus
%nonassoc '.'
%%

Module : ModuleAttributes
         ModuleOptions
         TopLevelDeclarations
         { Mod $1 $2 $3 }

TopLevelDeclarations : {- empty -}        { [] }
                     | TopLevelDeclaration TopLevelDeclarations { $1 : $2 }

TopLevelDeclaration : Visibility 'Type' VAR eol
                                     UserTypeDeclFields
                                 'End' 'Type' eol
                                       { UserTypeDecl $1 $3 $5 }
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
                    | Visibility GlobalVarDecl eol  { GlobalVarDecl $1 $2 }

Visibility : 'Private' { Private }
           | 'Public'  { Public }

UserTypeDeclFields : UserTypeDeclField                 { [$1] }
                   | UserTypeDeclField UserTypeDeclFields   { $1 : $2 }

UserTypeDeclField : UserTypeDeclArg eol { $1 }

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

FnDeclArgs : {- empty -}                        { [] }
           | FnDeclArg                          { [$1] }
           | FnDeclArg ',' FnDeclArgsContinued  { $1 : $3 }

FnDeclArgsContinued : FnDeclArg                          { [$1] }
                    | FnDeclArg ',' FnDeclArgsContinued  { $1 : $3 }

FnDeclArg : VAR 'As' TypeRef                     { Unspecified $ FuncArgDeclField $1 $3 }
          | VAR '(' ')' 'As' TypeRef             { Unspecified $ FuncArgDeclFieldArray $1 $5 }
          | 'ByRef' VAR 'As' TypeRef             { ByRef $ FuncArgDeclField $2 $4 }
          | 'ByRef' VAR '(' ')' 'As' TypeRef     { ByRef $ FuncArgDeclFieldArray $2 $6 }
          | 'ByVal' VAR 'As' TypeRef             { ByVal $ FuncArgDeclField $2 $4 }
          | 'ByVal' VAR '(' ')' 'As' TypeRef     { ByVal $ FuncArgDeclFieldArray $2 $6 }

DimDeclArgs : DimDeclArg                  { [$1] }
            | DimDeclArg ',' DimDeclArgs  { $1 : $3 }

UserTypeDeclArg : VAR 'As' TypeRef            { UserTypeDeclField $1 $3 }
                | VAR '(' ')' 'As' TypeRef    { UserTypeDeclFieldArray $1 $5 }
                | VAR '(' INT ')' 'As' TypeRef
                                              { UserTypeDeclFieldArrayWithUpperBound $1 $3 $6 }

GlobalVarDecl : VAR 'As' TypeRef            { GlobalTypeDecl $1 $3 }
              | VAR '(' ')' 'As' TypeRef    { GlobalTypeDeclArray $1 $5 }
              | VAR '(' INT ')' 'As' TypeRef
                                            { GlobalTypeDeclArrayWithUpperBound $1 $3 $6 }
              | VAR '(' INT 'To' INT ')' 'As' TypeRef
                                            { GlobalTypeDeclArrayWithBounds $1 $3 $5 $8 }

DimDeclArg : VAR 'As' TypeRef            { StmtTypeDecl $1 $3 }
           | VAR '(' ')' 'As' TypeRef    { StmtTypeDeclArray $1 $5 }
           | VAR '(' INT ')' 'As' TypeRef
                                         { StmtTypeDeclArrayWithUpperBound $1 $3 $6 }
           | VAR '(' INT 'To' INT ')' 'As' TypeRef
                                         { StmtTypeDeclArrayWithBounds $1 $3 $5 $8 }

Statements : {- empty -}              { [] }
           | Statement Statements     { $1 : $2 }

Statement : 'Dim' DimDeclArgs eol         { StmtDecl $2 }
          | 'LSet' Lhs '=' Expr eol       { StmtLSetAssign $2 $4 }
          | 'Set' Lhs '=' Expr eol        { StmtSetAssign $2 $4 }
          | Lhs '=' Expr eol              { StmtAssign $1 $3 }
          | VAR ':' eol                   { StmtLabel $1 }
          | 'GoTo' VAR eol                { StmtGoTo $2 }
          | 'If' Expr 'Then' eol
                Statements
            'Else' eol
                Statements
            'End' 'If' eol                { StmtIfThenElse $2 $5 $8 }
          | 'If' Expr 'Then' eol
                Statements
            'End' 'If' eol                { StmtIfThenElse $2 $5 [] }
          | 'For' Lhs '=' Expr 'To' Expr 'Step' Expr eol
                Statements
            'Next' VAR eol                { StmtFor $2 $4 $6 (Just $8) $10 }
          | 'For' Lhs '=' Expr 'To' Expr eol
                Statements
            'Next' VAR eol                { StmtFor $2 $4 $6 Nothing $8 }
          | 'Exit' 'Function' eol         { StmtReturn }
          | 'Call' VAR FNCallArgumentList eol
                                          { StmtCall (NameLhs $2) $3 }
          | 'Call' VAR '.' VAR FNCallArgumentList eol
                                          { StmtCall (FieldLhs [NameLhs $2, NameLhs $4]) $5 }
          | 'Call' VAR '.' VAR '.' VAR FNCallArgumentList eol
                                          { StmtCall (FieldLhs [NameLhs $2, NameLhs $4, NameLhs $6]) $7 }
          | 'Do' eol
                Statements
            'Loop' 'While' Expr eol       { StmtDoStatementsLoopWhileCond $3 $6 }

FNCallArgumentList : {- empty -}          { [] }
                   | '(' ExprList ')'     { $2 }

Lhs :: { Lhs }
  : VAR                           { NameLhs $1 }
  | VAR '(' NonEmptyExprList ')'
                                  { ArrayLhs $1 $3 }
  | VAR '.' Lhs                   { FieldLhs (NameLhs $1 : [$3]) }
  | VAR '(' NonEmptyExprList ')' '.' Lhs
                                  { FieldLhs (ArrayLhs $1 $3 : [$6]) }

SimpleExpr :: { Expr }
     : '-' Expr %prec precUnaryMinus { ENeg $2 }
     | Expr 'And' Expr            { EOp And $1 $3 }
     | Expr 'Or' Expr             { EOp Or $1 $3 }
     | Expr '<' Expr              { EOp LTh $1 $3 }
     | Expr '>' Expr              { EOp GrT $1 $3 }
     | Expr '<=' Expr             { EOp LEQ $1 $3 }
     | Expr '>=' Expr             { EOp GEQ $1 $3 }
     | Expr '+' Expr              { EOp Add $1 $3 }
     | Expr '-' Expr              { EOp Sub $1 $3 }
     | Expr '*' Expr              { EOp Mul $1 $3 }
     | Expr '/' Expr              { EOp Div $1 $3 }
     | '(' Expr ')'               { $2 }
     | Lit                        { ELit $1 }

Expr :: { Expr }
Expr : SimpleExpr                                 { $1 }
     | VAR                                        { EVar $1 }
     | VAR '(' ')'                                { ECall (NameLhs $1) [] }
     | VAR '(' NonEmptyExprList ')'               { ECall (NameLhs $1) $3 }
     | VAR '.' VAR                                { EAccess [NameLhs $1, NameLhs $3] }
     | VAR '.' VAR '(' NonEmptyExprList ')'       { ECall (FieldLhs [NameLhs $1, NameLhs $3]) $5 }
     | VAR '(' NonEmptyExprList ')' '.' VAR       { EAccess [ArrayLhs $1 $3, NameLhs $6] }
     | VAR '.' VAR '.' VAR                        { EAccess [NameLhs $1, NameLhs $3, NameLhs $5] }

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
  rawParseModule (remDupEOLs input)

-- todo rename to lexTokens
parseTokens :: String -> Either String [Token]
parseTokens = runExcept . lexStringToTokens

}
