module Main where

import Lib

import Syntax (Module)
import Parser (parseModule, parseTokens)

import Control.Monad.Trans
import System.Console.Haskeline

process :: String -> IO ()
process input = do
  let tokens = parseTokens input
  putStrLn ("Tokens: " ++ show tokens)
  let ast = parseModule input
  putStrLn ("Syntax: " ++ show ast)
  -- case ast of
  --   Left err -> do
  --     putStrLn "Parse Error:"
  --     print err
  --   Right ast -> show ast

main :: IO ()
main = loop
  where
  loop = do
    minput <- getContents
    (liftIO $ process minput)
