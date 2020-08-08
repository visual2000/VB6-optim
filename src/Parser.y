-- -*- mode: prog -*-
{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser (
  parseModule,
  parseTokens,
) where

import Lexer
import AG

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
    'Declare'     { Token _ (TokenDeclare) }
    'Call'        { Token _ (TokenCall) }
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
          | 'For' Lhs '=' Expr 'To' Expr 'Step' Expr eol
                Statements
            'Next' VAR eol                { StmtFor $2 $4 $6 (Just $8) $10 }
          | 'For' Lhs '=' Expr 'To' Expr eol
                Statements
            'Next' VAR eol                { StmtFor $2 $4 $6 Nothing $8 }
          | 'Exit' 'Function' eol         { StmtReturn }
          | 'Call' FNCallRef FNCallArgumentList eol
                                          { StmtCall $2 $3 }
          | 'Do' eol
                Statements
            'Loop' 'While' Expr eol       { StmtDoStatementsLoopWhileCond $3 $6 }

FNCallArgumentList : {- empty -}          { [] }
                   | '(' ExprList ')'     { $2 }

Withs : With { [$1] }
      | With Withs { $1 : $2 }

With : '.' Lhs '=' Expr eol { WithAssignment $2 $4 }

Lhs : VAR                           { NameLhs $1 }
    | DottedLhs                     { FieldLhs $1 }
    | VAR '(' NonEmptyExprList ')'
                                    { ArrayLhs $1 $3 }

DottedLhs : Lhs '.' Lhs
                                    { $1 : [$3] }

DottedFuncCall : VAR '.' DottedFuncCall
                                    { NameLhs $1 : $3 }
               | VAR                { [NameLhs $1] }

FNCallRef : DottedFuncCall        { FieldLhs $1 }

Expr : FNCallRef '(' ExprList ')' { ECall $1 $3 }
     | '-' Expr                   { ENeg $2 }
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
     | DottedFuncCall             { EAccess $1 }

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
