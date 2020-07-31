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
    Left err -> putStrLn err
    Right lexedTokens -> do
      putStrLn ("Tokens: " ++ show lexedTokens)
      let ast = parseModule lexedTokens
      -- putStrLn ("Syntax: " ++ show ast) -- todo bring back raw tokens display
      case ast of
        Left err -> do
          putStrLn "Parse Error:"
          putStrLn (err input)
        Right ast -> putStrLn $ render $ pp ast

main :: IO ()
main = do minput <- getContents
          liftIO $ process minput
