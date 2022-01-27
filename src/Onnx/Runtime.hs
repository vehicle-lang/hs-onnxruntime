module Onnx.Runtime where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Pool

import Onnx.Runtime.Internal

ortVersion :: IO String
ortVersion = do
  ortApiBasePtr <- c_GetOrtApiBase
  versionString <- c_OrtApiBase_GetVersionString ortApiBasePtr
  peekCString versionString