{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module OnnxRuntime where

import Foreign
import Foreign.C.Types
import Foreign.C.String

#include <onnxruntime/core/session/onnxruntime_c_api.h>

newtype OrtApiBase = OrtApiBase (Ptr OrtApiBase)

foreign import ccall unsafe "onnxruntime/core/session/onnxruntime_c_api.h OrtGetApiBase"
  c_OrtGetApiBase :: IO (Ptr OrtApiBase)

