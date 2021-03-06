module Main where

import Lib
import Util

import AG
import Printer (Printable, printModule)
import Parser (parseModule, parseTokens)

import Control.Monad (when)
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

parseStringToModule :: String -> IO Module
parseStringToModule input = do
  let tokens = parseTokens input
  case tokens of
    Left err -> do
      putStrLn err
      return $ error "Lexing error."
    Right lexedTokens -> do
      -- putStrLn ("Tokens: " ++ show lexedTokens)
      let ast = parseModule lexedTokens
      case ast of
        Left err -> do
          putStrLn (err input)
          return $ error "Parsing error."
        Right ast -> do
          -- putStrLn ("Syntax: " ++ show ast)
          return ast

parseFileToModule :: FilePath -> IO Module
parseFileToModule f = do fileContents <- readFile f
                         hPutStr stderr $ "Reading " ++ f ++ "...\n"
                         parseStringToModule fileContents

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
  let allDecls = concat [ decls | (Mod _ _ decls) <- ms ] in
    Mod [ModuleAttribute "VB_Name" (LString newModuleName)]
        [OptionExplicit]
        (
          getDllFuncRefs allDecls
          ++ getUserTypeDecls allDecls
          ++ getGlobalVarDecls allDecls
          ++ getFuncDecls allDecls
          ++ getSubDecls allDecls
        )

-- TODO notice and copy *.frx files.

copyOtherFiles :: Project -> FilePath -> IO()
copyOtherFiles p dest =
  let other_src = otherAssets p
      base = baseDirectory p in
    do sequence_ [ copyFile (base </> f) (dest </> f) | f <- otherAssets p ]

projectFile = "/Users/paul/Public/BasicTrace/BasicTrace.vbp"
-- projectFile = "/Users/paul/Public/Microbenchmarks/Microbenchmarks.vbp"
outDirectory = "/Users/paul/Public/OptimTrace"
cleanTargetDir = True
newModuleName = "Monolith"
generatedSubName = "OurMagicEntryPoint"

parseModuleList :: FilePath -> [FilePath] -> IO [Module]
parseModuleList baseDir = mapM (parseFileToModule . (baseDir </>))

formatVbpProjectConfig :: Ini -> String
formatVbpProjectConfig i = let globals = iniGlobals i in
                  eolsToCRLF $ concatMap (\(k,v) -> T.unpack k ++ "=" ++ T.unpack v ++ "\n") globals
                               ++ "\n"
                               ++ (T.unpack $ printIniWith (WriteIniSettings EqualsKeySeparator) i)

processProject :: Project -> FilePath -> IO()
processProject project dest = do
  when cleanTargetDir $ doesDirectoryExist dest >>= \exist -> when exist $ removeDirectoryRecursive dest
  createDirectory dest
  copyOtherFiles project dest
  do mods <- parseModuleList (baseDirectory project) (modules project)
     let finalModule = printModule $ (getSingleSub . getCallsiteFree . getDimLifted) (combineModules mods) in
       do putStrLn finalModule
          writeFile (dest </> newModuleName ++ ".bas") finalModule
  let ini = originalIni project
      globalsMinusModules = filter (\(k,v) -> T.unpack k /= "Module") $ iniGlobals ini
      newini = ini { iniGlobals = (T.pack "Module", T.pack $ newModuleName ++ "; " ++ newModuleName ++ ".bas"):globalsMinusModules } in
    writeFile (dest </> projectName project ++ ".vbp") (formatVbpProjectConfig newini)

main :: IO ()
main = do fileContents <- T.readFile projectFile
          hPutStrLn stderr $ "Reading project " ++ projectFile ++ "..."
          absDir <- makeAbsolute projectFile
          let p = parseProject (takeDirectory absDir) projectFile fileContents
          case p of
            Nothing -> do hPutStrLn stderr "Couldn't parse project file.\n"
                          exitFailure
            Just proj -> do hPutStrLn stderr "...done."
                            sequence_ [ putStrLn $ "Found module: " ++ m | m <- modules proj ]
                            sequence_ [ putStrLn $ "Found other source: " ++ m | m <- otherAssets proj ]
                            processProject proj outDirectory
          exitSuccess

-- TODO warn/bail if final statement in Func isn't a return/Exit
-- Function (because we use that to keep track of recursion depth).

getSingleSub :: Module -> Module
getSingleSub m = all_statements_in_one_sub_Syn_Module (wrapAG m)

getDimLifted :: Module -> Module
getDimLifted m = dim_lifted_Syn_Module (wrapAG m)

getCallsiteFree :: Module -> Module
getCallsiteFree m = with_initialising_Syn_Module (wrapAG m)

wrapAG :: Module -> Syn_Module
wrapAG m = wrap_Module (sem_Module m) Inh_Module{counter_Inh_Module=0
                                                ,generated_sub_name_Inh_Module=generatedSubName}
