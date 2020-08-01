module Main where

import Lib

import Syntax (Module)
import Printer (Printable, printModule)
import Parser (parseModule, parseTokens)

import Control.Monad.Trans
import Control.Monad.Except
import System.Console.Haskeline

import Data.Ini
import Data.Text (pack, unpack)

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
          putStrLn $ printModule ast

projectFile = "/Users/paul/Public/BasicTrace/BasicTrace.vbp"

files = [ "examples/HitFuncs.bas"
        , "examples/CameraFuncs.bas"
        , "examples/GfxInit.bas"
        , "examples/GfxPrimitives.bas"
        , "examples/Logging.bas"
        , "examples/VecFuncts.bas"
        , "examples/Types.bas"
        -- , "examples/RayFuncs.bas"
        -- , "examples/NumericalConstants.bas"
        ]

parseFile :: FilePath -> IO()
parseFile f = do fileContents <- readFile f
                 hPutStr stderr $ "Reading " ++ f ++ "...\n"
                 mdl <- process fileContents
                 return ()

parseProject :: FilePath -> IO (Maybe Ini, [FilePath])
parseProject p = do hPutStrLn stderr $ "Reading project " ++ p ++ "..."
                    fileContents <- readIniFile p
                    case fileContents of
                      Left err -> do hPutStrLn stderr "Couldn't parse project file!"
                                     return (Nothing, [])
                      Right ini -> do
                        let glo = iniGlobals ini
                        let mods = map (unpack . snd) $ filter (\(k,v)->(unpack k)=="Module") glo
                        let stripped = map ((\ws->ws!!1) . words) mods
                        putStrLn (stripped >>= (\m -> "Found module: " ++ m ++ ".\n"))
                        return (Just ini, stripped)

main = parseProject projectFile
-- main :: IO ()
-- main = mapM_ parseFile files
