module Main where

import Lib

import Syntax (Module)
import Printer (toDoc)
import Parser (parseModule, parseTokens)

import Control.Monad.Trans
import System.Console.Haskeline

import Text.PrettyPrint

process :: String -> IO ()
process input = do
  let tokens = parseTokens input
  putStrLn ("Tokens: " ++ show tokens)
  let ast = parseModule input
  putStrLn ("Syntax: " ++ show ast)
  case ast of
    Left err -> do
      putStrLn "Parse Error:"
      print err
    Right ast -> putStr $ render $ toDoc ast

main :: IO ()
main = loop
  where
  loop = do
    minput <- getContents
    (liftIO $ process minput)
