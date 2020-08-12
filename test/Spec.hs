import Parser
import Printer

import AG
import Util (getDimLifted, getCallsiteFree)

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
  let toks = parseTokens contents
  case toks of
    Left err -> do
      putStrLn err
      return $ error "Lexing error."
    Right toks' -> do
      let mod = parseModule toks'
      case mod of
        Left err -> do
          putStrLn (err contents)
          return $ error "Parsing error."
        Right ast -> do
          return ast

parseAndPrettyPrintFile :: FilePath -> IO String
parseAndPrettyPrintFile f = do
  contents <- readFile f
  let toks = parseTokens contents
  case toks of
    Left err -> error "halp"
    Right toks' -> do
      let mod = parseModule toks'
      case mod of
        Left err -> error "Halp"
        Right ast -> do
          let result = printModule ast
          return result
