module Onnx.Runtime where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Pool

import Onnx.Runtime.Internal

ortVersion :: IO String
ortVersion = do
  ortApiBase <- c_GetOrtApiBase
  c_OrtApiBase_GetVersionString ortApiBase

createOrtEnv :: IO ()
createOrtEnv = do
  ortApiBase <- c_GetOrtApiBase
  ortApi <- c_OrtApiBase_GetOrtApi ortApiBase
  withPool $ \pool -> do
    ortEnvPtr <- pooledMalloc pool
    let ortEnv = OrtEnv ortEnvPtr
    status <- c_OrtApi_CreateEnv ortApi OrtLoggingLevelVerbose "H" ortEnvPtr
    print ortEnvPtr
    -- print status
    c_OrtApi_ReleaseEnv ortApi ortEnv