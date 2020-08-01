module Main where

import Lib

import Syntax (Module)
import Printer (Printable, printModule)
import Parser (parseModule, parseTokens)

import Control.Monad.Trans
import Control.Monad.Except
import System.Console.Haskeline
import System.Exit

import Data.Ini
import qualified Data.Text    as T
import qualified Data.Text.IO as T

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

data Project = Project { originalIni :: Ini
                       , modules :: [FilePath]
                       , otherAssets :: [FilePath]
                       }
  deriving (Show, Eq)

parseProject :: T.Text -> Maybe Project
parseProject p = let ini = parseIni p in
                   case ini of
                     Left err -> Nothing
                     Right ini' ->
                       let glo = iniGlobals ini' in
                         let mods = map (T.unpack . snd) $ filter (\(k,v)-> T.unpack k =="Module") glo in
                           let stripped = map ((!!1) . words) mods in
                             Just $ Project{ originalIni = ini'
                                           , modules = stripped
                                           , otherAssets = []
                                           }



main :: IO ()
main = do fileContents <- T.readFile projectFile
          hPutStrLn stderr $ "Reading project " ++ projectFile ++ "..."
          let p = parseProject fileContents
          case p of
            Nothing -> do hPutStrLn stderr "Couldn't parse project file.\n"
                          exitFailure
            Just proj -> do hPutStrLn stderr "...done."
                            sequence [ putStrLn $ "Found module: " ++ m | m <- modules proj ]
          exitSuccess
