module Main where

import Lib

import Syntax (Module)
import Printer (Printable, pp)
import Parser (parseModule, parseTokens)

import Control.Monad.Trans
import Control.Monad.Except
import System.Console.Haskeline

import System.IO

import Text.PrettyPrint

process :: String -> IO ()
process input = do
  let tokens = parseTokens input
  case tokens of
    Left err -> do
      putStrLn "Lexing error:"
      putStrLn err
    Right lexedTokens -> do
      -- putStrLn ("Tokens: " ++ show lexedTokens)
      let ast = parseModule lexedTokens
      case ast of
        Left err -> do
          putStrLn "Parse error:"
          putStrLn (err input)
        Right ast -> do
          -- putStrLn ("Syntax: " ++ show ast)
          putStrLn $ render $ pp ast

files = [ "examples/HitFuncs.bas"
        , "examples/CameraFuncs.bas"
        , "examples/GfxInit.bas"
        , "examples/GfxPrimitives.bas"
        , "examples/Logging.bas"
        , "examples/VecFuncts.bas"
        , "examples/Types.bas"
        -- , "examples/RayFuncs.bas"
        -- , "examples/Numericalconstants.bas"
        ]

parseFile :: FilePath -> IO()
parseFile f = do fileContents <- readFile f
                 hPutStr stderr $ "Reading " ++ f ++ "...\n"
                 mdl <- process fileContents
                 return ()

main :: IO ()
main = mapM_ parseFile files
