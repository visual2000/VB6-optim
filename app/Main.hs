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
  case tokens of
    Left err -> do
      putStrLn "Lexing error:"
      putStrLn err
    Right lexedTokens -> do
      putStrLn ("Tokens: " ++ show lexedTokens)
      let ast = parseModule lexedTokens
      case ast of
        Left err -> do
          putStrLn "Parse error:"
          putStrLn (err input)
        Right ast -> do
          putStrLn ("Syntax: " ++ show ast)
          putStrLn $ render $ pp ast

main :: IO ()
main = do minput <- getContents
          liftIO $ process minput
