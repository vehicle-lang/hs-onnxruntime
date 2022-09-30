import Distribution.Simple hiding (Version)
import Distribution.Simple.Setup
import Distribution.Simple.Utils (rawSystemExit, rawSystemIOWithEnv, die', copyFileTo, rawSystemStdout, dieNoVerbosity, dieWithLocation', createDirectoryIfMissingVerbose, copyFiles, copyFileVerbose)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo (..))
import Distribution.PackageDescription (PackageDescription)

import Control.Monad (when, (<=<))
import System.Exit (exitWith)
import System.IO (stdout, hFlush)
import qualified System.Info
import Debug.Trace (trace, traceShow)
import qualified Distribution.Verbosity as Verbosity
import Distribution.Verbosity
import Data.List (stripPrefix)
import Data.Version (Version(..), showVersion, makeVersion, parseVersion)
import Text.Read (readMaybe)
import Text.ParserCombinators.ReadP (readP_to_S, skipSpaces)
import Data.Char (isSpace)
import Text.ParserCombinators.ReadPrec (minPrec, readPrec_to_S)
import Distribution.Simple.InstallDirs (InstallDirs(..))
import Distribution.Simple.LocalBuildInfo (absoluteInstallDirs)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { preConf  = \a c ->
      checkCMakeVersion a c >>
      checkPythonVersion a c >>
      gitUpdateSubmodules a c >>
      preConf simpleUserHooks a c

  , preBuild = \a b ->
      buildOnnxRuntime (getBuildVerbosity b) >>
      preBuild simpleUserHooks a b

  , copyHook = \p l u c ->
      copyOnnxRuntime p l u c >>
      copyHook simpleUserHooks p l u c
  }

-------------------------------------------------------------------------------
-- Settings

(</>) :: FilePath -> FilePath -> FilePath
(</>) a b = a <> (if System.Info.os == "mingw32" then "\\" else "/") <> b

onnxruntimeDir :: FilePath
onnxruntimeDir = "vendor" </> "onnxruntime"

onnxruntimeBuildDir :: FilePath
onnxruntimeBuildDir = onnxruntimeDir </> "build" </> subfolder
  where
    subfolder :: FilePath
    subfolder
      | System.Info.os == "mingw32" = "Windows"
      | System.Info.os == "darwin"  = "MacOS"
      | otherwise                   = "Linux"

pythonProg :: FilePath
pythonProg = if System.Info.os == "mingw32" then "python" else "python3"

-------------------------------------------------------------------------------
-- Pre-configuration

gitUpdateSubmodules :: Args -> ConfigFlags -> IO ()
gitUpdateSubmodules _args flags = do
  rawSystemExit (getConfigVerbosity flags) "env"
    ["git", "submodule", "update", "--init"]

checkPythonVersion :: Args -> ConfigFlags -> IO ()
checkPythonVersion args flags = do
  checkVersion args flags pythonProg (makeVersion [3,6]) (parseVersionMaybe <=< stripPrefix "Python ")

checkCMakeVersion :: Args -> ConfigFlags -> IO ()
checkCMakeVersion args flags = do
  checkVersion args flags "cmake" (makeVersion [3,18]) (parseVersionMaybe <=< stripPrefix "cmake version ")

checkVersion :: Args -> ConfigFlags -> FilePath -> Version -> (String -> Maybe Version) -> IO ()
checkVersion args flags prog minimumVersion parseVersion = do
  let verbosity = getConfigVerbosity flags

  versionOutput <- rawSystemStdout verbosity prog ["--version"]

  let notFoundError output = unlines
        [ "Building OnnxRuntime requires '" <> prog <> "' " <> showVersion minimumVersion <>
          " or above on the system path."
        , "Found the following output when trying to get the '" <> prog <> "' version:"
        , show output
        ]

  firstLineVersionOutput <- case lines versionOutput of
    []      -> dieNoVerbosity $ notFoundError versionOutput
    (l : _) -> return l

  case parseVersion firstLineVersionOutput of
    Nothing -> dieNoVerbosity $ notFoundError firstLineVersionOutput

    Just version ->
      when (version < minimumVersion) $
        dieNoVerbosity $ unlines
          [ "Building OnnxRuntime requires '" <> prog <> "' " <> showVersion minimumVersion <>
            " or above on the system path."
          , "Found version " <> versionOutput
          ]

-------------------------------------------------------------------------------
-- Build

buildOnnxRuntime :: Verbosity -> IO ()
buildOnnxRuntime verbosity = do
  putStrLn "\nBuilding ONNX runtime libraries"

  rawSystemExitIn verbosity onnxruntimeDir pythonProg
    [ "tools" </> "ci_build" </> "build.py"
    , "--build_dir", onnxruntimeBuildDir
    , "--config", "RelWithDebInfo"
    , "--build_shared_lib"
    , "--parallel"
    , "--skip_tests"
    ]

-------------------------------------------------------------------------------
-- Copy

copyOnnxRuntime :: PackageDescription -> LocalBuildInfo -> UserHooks -> CopyFlags -> IO ()
copyOnnxRuntime packageDescription localBuildInfo _ flags = do
  versionStr <- getOnnxruntimeVersion
  let verbosity = getCopyVerbosity flags

  let sourceDirectory = onnxruntimeBuildDir </> "RelWithDebInfo"

  let copyDestination = fromFlag . copyDest $ flags
  let installDirs = absoluteInstallDirs packageDescription localBuildInfo copyDestination
  let targetDirectory = libdir installDirs

  let sourceLibName = "libonnxruntime." <> sharedLibExtension
  let targetLibName = "libonnxruntime." <> sharedLibExtension

  let sourceFile = sourceDirectory </> sourceLibName
  let targetFile = targetDirectory </> targetLibName

  putStrLn "\nCopying ONNX runtime libraries"
  putStrLn $ "From: " <> sourceFile
  putStrLn $ "To: " <> targetFile

  copyFileVerbose verbosity sourceFile targetFile
  copyFileVerbose verbosity (sourceFile <> "." <> versionStr) (targetFile <> "." <> versionStr)

getOnnxruntimeVersion :: IO String
getOnnxruntimeVersion = do
  result <- readFile (onnxruntimeDir </> "VERSION_NUMBER")
  case lines result of
    l : _ -> return l
    []    -> dieNoVerbosity "Could not read version from OnnxRuntime repo"

-------------------------------------------------------------------------------
-- Utilities

rawSystemExitIn :: Verbosity -> FilePath -> FilePath -> [String] -> IO ()
rawSystemExitIn verbosity workingDirectory command commandArgs = do
  _exitCode <- rawSystemIOWithEnv
    verbosity
    command
    commandArgs
    (Just workingDirectory)
    Nothing
    Nothing
    Nothing
    Nothing
  return ()

parseVersionMaybe :: String -> Maybe Version
parseVersionMaybe s =
  case [ v | (v,"") <- readP_to_S parseVersion' s ] of
    [v] -> Just v
    _   -> Nothing
  where
    parseVersion' = skipSpaces *> parseVersion <* skipSpaces

getCopyVerbosity :: CopyFlags -> Verbosity
getCopyVerbosity flags = fromFlagOrDefault Verbosity.normal $ copyVerbosity flags

getConfigVerbosity :: ConfigFlags -> Verbosity
getConfigVerbosity flags = fromFlagOrDefault Verbosity.normal $ configVerbosity flags

getBuildVerbosity :: BuildFlags -> Verbosity
getBuildVerbosity flags = fromFlagOrDefault Verbosity.normal $ buildVerbosity flags

sharedLibExtension :: FilePath
sharedLibExtension
  | System.Info.os == "mingw32" = "dll"
  | System.Info.os == "darwin"  = "dylib"
  | otherwise                   = "so"