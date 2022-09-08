import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Utils (rawSystemExit, rawSystemIOWithEnv, die', copyFileTo)

import System.Exit (exitWith)
import qualified System.Info
import Debug.Trace (trace)
import System.IO (stdout, hFlush)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
    { preBuild = \a b -> buildOnnxRuntime a b >> preBuild simpleUserHooks a b
    }

buildOnnxRuntime :: Args -> BuildFlags -> IO ()
buildOnnxRuntime args flags = do
  gitUpdateSubmodules args flags
  case System.Info.os of
    "mingw32" -> buildOnnxRuntimeWindows args flags

    "darwin"  -> buildOnnxRuntimePosix args flags
    "freebsd" -> buildOnnxRuntimePosix args flags
    "linux"   -> buildOnnxRuntimePosix args flags
    "netbsd"  -> buildOnnxRuntimePosix args flags
    "openbsd" -> buildOnnxRuntimePosix args flags

    osName -> die' (fromFlag $ buildVerbosity flags) $ "Unsupported operating system: " <> osName

gitUpdateSubmodules :: Args -> BuildFlags -> IO ()
gitUpdateSubmodules _args flags = do
  rawSystemExit (fromFlag $ buildVerbosity flags) "env"
    --["echo", "XXX"]
    ["git", "submodule", "update", "--init"]


buildOnnxRuntimeWindows :: Args -> BuildFlags -> IO ()
buildOnnxRuntimeWindows _args flags =
  rawSystemExitIn flags "vendor/onnxruntime" "./build.bat"
    ["--config", "RelWithDebInfo", "--build_shared_lib", "--parallel"]

buildOnnxRuntimePosix :: Args -> BuildFlags -> IO ()
buildOnnxRuntimePosix _args flags = do
  rawSystemExitIn flags "vendor/onnxruntime" "./build.sh"
    ["--config", "RelWithDebInfo", "--build_shared_lib", "--parallel"]


rawSystemExitIn :: BuildFlags -> FilePath -> FilePath -> [String] -> IO ()
rawSystemExitIn flags workingDirectory command commandArgs = do
  _exitCode <- rawSystemIOWithEnv
    (fromFlag $ buildVerbosity flags)
    command
    commandArgs
    (Just workingDirectory)
    Nothing
    Nothing
    Nothing
    Nothing
  return ()

