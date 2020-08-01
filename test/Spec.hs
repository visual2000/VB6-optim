import Parser
import Printer

import Text.PrettyPrint
import Test.Hspec

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

compareParsed f = do contents <- readFile f
                     testOutput <- parseAndPrettyPrintFile f
                     testOutput `shouldBe` contents

compareToLiteral f lit = do contents <- readFile f
                            testOutput <- parseAndPrettyPrintFile f
                            testOutput `shouldBe` lit

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
