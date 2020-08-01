import Parser
import Printer

import Text.PrettyPrint
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)
  describe "Module1 end-to-end" $ do
    it "should parse and print the same module contents" $ do
      contents <- readFile "examples/Module1.bas"
      test1Output <- test1
      test1Output `shouldBe` contents


test1 :: IO String
test1 = do contents <- readFile "examples/Module1.bas"
           let toks = parseTokens contents
           case toks of
             Left err -> error "halp"
             Right toks' -> do
               let mod = parseModule toks'
               case mod of
                 Left err -> error "Halp"
                 Right ast -> do
                   let result = render $ pp ast
                   return result
