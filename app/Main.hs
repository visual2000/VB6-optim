module Main where

import Lib
import Util

import Syntax
import Printer (Printable, printModule)
import Parser (parseModule, parseTokens)

import Control.Monad.Trans
import Control.Monad.Except
import System.Console.Haskeline
import System.Exit
import System.Directory
import System.FilePath

import Data.Ini
import qualified Data.Text    as T
import qualified Data.Text.IO as T

import System.IO

import Text.PrettyPrint

process :: String -> IO Module
process input = do
  let tokens = parseTokens input
  case tokens of
    Left err -> do
      putStrLn "Lexing error:"
      putStrLn err
      return $ error "asdf"
    Right lexedTokens -> do
      -- putStrLn ("Tokens: " ++ show lexedTokens)
      let ast = parseModule lexedTokens
      case ast of
        Left err -> do
          putStrLn "Parse error:"
          putStrLn (err input)
          return $ error "asdf"
        Right ast -> do
          -- putStrLn ("Syntax: " ++ show ast)
          return ast

parseFile :: FilePath -> IO Module
parseFile f = do fileContents <- readFile f
                 hPutStr stderr $ "Reading " ++ f ++ "...\n"
                 mdl <- process fileContents
                 return mdl

data Project = Project { originalIni :: Ini
                       , modules :: [FilePath]
                       , otherAssets :: [FilePath]
                       , baseDirectory :: FilePath
                       , projectName :: String
                       }
  deriving (Show, Eq)

parseProject :: FilePath -> FilePath -> T.Text -> Maybe Project
parseProject baseDir filename p = let ini = parseIni p in
                   case ini of
                     Left err -> Nothing
                     Right ini' ->
                       let glo = iniGlobals ini'
                           mods = map (T.unpack . snd) $ filter (\(k,v)-> T.unpack k =="Module") glo
                           projTitle = map (T.unpack . snd) $ filter (\(k,v)-> T.unpack k =="Title") glo
                           other_src = map (T.unpack . snd) $ filter (\(k,v)-> T.unpack k =="Form") glo
                           afterSemicolon = map ((!!1) . words) mods in
                             Just $ Project{ originalIni = ini'
                                           , modules = afterSemicolon
                                           , otherAssets = other_src
                                           , projectName = read $ head projTitle
                                           , baseDirectory = baseDir
                                           }


combineModules :: [Module] -> Module
combineModules ms =
  let allDecls = (concat [ decls | (Mod _ _ decls) <- ms ]) in
    Mod [Attribute "VB_Name" (LString "Monolith")]
        [OptionExplicit]
        (
          getDllFuncRefs allDecls
          ++ getTypeDefs allDecls
          ++ getGlobalVarDecls allDecls
          ++ getFuncDecls allDecls
          ++ getSubDecls allDecls
        )


copyOtherFiles :: Project -> FilePath -> IO()
copyOtherFiles p dest =
  let other_src = otherAssets p
      base = baseDirectory p in
    do sequence_ [ copyFile (base </> f) (dest </> f) | f <- otherAssets p ]

projectFile = "/Users/paul/Public/BasicTrace/BasicTrace.vbp"
outDirectory = "./output"

parseModuleList :: FilePath -> [FilePath] -> IO [Module]
parseModuleList baseDir fs = mapM parseFile (map (\f -> baseDir </> f) fs)

ourPrintIni :: Ini -> String
ourPrintIni i = let globals = iniGlobals i in
                  fixDOSEOL $
                  concat (map (\(k,v) -> T.unpack k ++ "=" ++ T.unpack v ++ "\n") globals)
                  ++ "\n"
                  ++ (T.unpack $ printIniWith (WriteIniSettings EqualsKeySeparator) i)

doTheThing :: Project -> FilePath -> IO()
doTheThing project dest = do
  createDirectory dest
  copyOtherFiles project dest
  let allmodulefiles = modules project in
    do allparsedmodules <- parseModuleList (baseDirectory project) allmodulefiles
       let bigModule = combineModules allparsedmodules in
         writeFile (dest </> "Monolith.bas") (printModule bigModule)
  let oldini = originalIni project
      newglobals = filter (\(k,v) -> T.unpack k /= "Module") $ iniGlobals oldini
      newini = oldini { iniGlobals = (T.pack "Module", T.pack "Monolith; Monolith.bas"):newglobals } in
    writeFile (dest </> (projectName project) ++ ".vbp") (ourPrintIni newini)

main :: IO ()
main = do fileContents <- T.readFile projectFile
          hPutStrLn stderr $ "Reading project " ++ projectFile ++ "..."
          absDir <- makeAbsolute projectFile
          let p = parseProject (takeDirectory absDir) projectFile fileContents
          case p of
            Nothing -> do hPutStrLn stderr "Couldn't parse project file.\n"
                          exitFailure
            Just proj -> do hPutStrLn stderr "...done."
                            sequence [ putStrLn $ "Found module: " ++ m | m <- modules proj ]
                            sequence [ putStrLn $ "Found other source: " ++ m | m <- otherAssets proj ]
                            doTheThing proj outDirectory
          exitSuccess
