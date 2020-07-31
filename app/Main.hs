module Main where

import Lib

import Syntax (Module)
import Printer (Printable, pp)
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
      putStrLn err
    Right ast -> putStrLn $ render $ pp ast

main :: IO ()
main = do minput <- getContents
          liftIO $ process minput
