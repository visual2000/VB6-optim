import Lexer
import Parser
import Printer

import AG
import Util (getDimLifted, getCallsiteFree)

import Control.Monad.Except
import Text.PrettyPrint
import Test.Hspec

-- As a workaround, to pretty-print modules.  Without this, multiline
-- strings are printed as "asd\nafasdf\n..." which is unreadable.
-- See also: https://github.com/hspec/hspec/issues/384
newtype TestString = TestString String
  deriving Eq

instance Show TestString where
  show (TestString s) = s

main :: IO ()
main = hspec $ do
  describe "Module1 end-to-end" $ do
    it "should parse and print the same module contents" $ do
      compareParsed "examples/Module1.bas"

  describe "Line continuations work" $ do
    it "works" $ do
      compareToLiteral "examples/LineContinuation.bas" "' ---\r\n' ---\r\nPublic Function SillyFunc(x As Long, y As Boolean) As Boolean\r\nEnd Function\r\n' ---\r\n' The end\r\n"

  describe "HitFuncs.bas end-to-end" $ do
    it "is the same" $ do
      compareParsed "examples/HitFuncs.bas"

  describe "verify the ordering of argument-setting vs return-value-getting" $ do
    it "corresponds to fixture" $ do
      compareToExpected "examples/ReturnValOrdering.bas" "examples/ReturnValOrdering.expect.bas"

  describe "statement parsing" $ do
    it "manages simple assignment" $ do
      parsed <- tryParse rawParseStatement "x = foo\n"
      parsed `shouldBe` StmtAssign (NameLhs "x") (EVar "foo")

    it "manages assignment of fields" $ do
      parsed <- tryParse rawParseStatement "x.y.z = foo\n"
      parsed `shouldBe` StmtAssign (FieldLhs [NameLhs "x",
                                              FieldLhs [NameLhs "y",
                                                        NameLhs "z"]]) (EVar "foo")

    it "manages assignment of arrays" $ do
      parsed <- tryParse rawParseStatement "x(1, 4) = foo\n"
      parsed `shouldBe` StmtAssign (ArrayLhs "x" [ELit (LInt 1),
                                                  ELit (LInt 4)]) (EVar "foo")

    it "manages assignment of arrays mixed with fields" $ do
      parsed <- tryParse rawParseStatement "x.y(3, 5) = foo\n"
      parsed `shouldBe` StmtAssign (FieldLhs [NameLhs "x",
                                              ArrayLhs "y" [ELit (LInt 3),
                                                            ELit (LInt 5)]]) (EVar "foo")

    it "manages assignment of arrays mixed with fields, non-constant values" $ do
      parsed <- tryParse rawParseStatement "x.y(LBound(y), (z + 4)) = foo\n"
      parsed `shouldBe` StmtAssign (FieldLhs [NameLhs "x",
                                              ArrayLhs "y" [ECall (NameLhs "LBound") [EVar "y"],
                                                            EOp Add (EVar "z") (ELit (LInt 4))]]) (EVar "foo")

    it "parses assignment from function call" $ do
      parsed <- tryParse rawParseStatement "return_add = TVec3_init((arg_add_0(rec_depth_add).x + arg_add_1(rec_depth_add).x), (arg_add_0(rec_depth_add).y + arg_add_1(rec_depth_add).y), (arg_add_0(rec_depth_add).z + arg_add_1(rec_depth_add).z))\n"
      parsed `shouldBe`
        StmtAssign (NameLhs "return_add")
                     (ECall (NameLhs "TVec3_init")
                      [EOp Add
                       (EAccess [ArrayLhs "arg_add_0" [EVar "rec_depth_add"],NameLhs "x"])
                       (EAccess [ArrayLhs "arg_add_1" [EVar "rec_depth_add"],NameLhs "x"]),
                       EOp Add
                       (EAccess [ArrayLhs "arg_add_0" [EVar "rec_depth_add"],NameLhs "y"])
                       (EAccess [ArrayLhs "arg_add_1" [EVar "rec_depth_add"],NameLhs "y"]),
                       EOp Add
                       (EAccess [ArrayLhs "arg_add_0" [EVar "rec_depth_add"],NameLhs "z"])
                       (EAccess [ArrayLhs "arg_add_1" [EVar "rec_depth_add"],NameLhs "z"])])



tryParse :: ([Token] -> Except (String -> String) a) -> String -> IO a
tryParse parseWith str = case parseTokens str of
                           Left err -> do
                             putStrLn err
                             return $ error "Lexing error."
                           Right toks -> do
                             case runExcept (parseWith $ remDupEOLs toks) of
                               Left err -> do
                                 putStrLn (err str)
                                 return $ error "Parsing error."
                               Right ast -> do
                                 return ast

compareParsed f = do contents <- readFile f
                     testOutput <- parseAndPrettyPrintFile f
                     testOutput `shouldBe` contents

compareToLiteral f lit = do contents <- readFile f
                            testOutput <- parseAndPrettyPrintFile f
                            testOutput `shouldBe` lit

compareToExpected f f_expect = do original <- parseFile f
                                  expected <- parseFile f_expect
                                  myShowModule (optimiseModule expected) `shouldBe` myShowModule original

myShowModule :: Module -> TestString
myShowModule = TestString . printModule

optimiseModule :: Module -> Module
optimiseModule = getCallsiteFree . getDimLifted

parseFile :: FilePath -> IO Module
parseFile f = do
  contents <- readFile f
  tryParse rawParseModule contents

parseAndPrettyPrintFile :: FilePath -> IO String
parseAndPrettyPrintFile f = do mod <- parseFile f
                               let result = printModule mod
                               return result
