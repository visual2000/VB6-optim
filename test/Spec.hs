import Parser
import Printer

import Text.PrettyPrint
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Module1 end-to-end" $ do
    it "should parse and print the same module contents" $ do
      contents <- readFile "examples/Module1.bas"
      test1Output <- parseAndPrettyPrintFile "examples/Module1.bas"
      test1Output `shouldBe` contents


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
