{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module OnnxRuntime where

import Foreign
import Foreign.C.Types
import Foreign.C.String

#include "onnxruntimeHS.h"


-- * OrtApiBase

ortApiVersion :: Int
ortApiVersion = #const ORT_API_VERSION

newtype OrtApiBase = OrtApiBase { unOrtApiBase :: Ptr OrtApiBase }

foreign import ccall unsafe "OrtGetApiBase"
  c_OrtGetApiBase :: IO (Ptr OrtApiBase)

c_OrtApiBase_GetApi :: Ptr OrtApiBase -> Ptr OrtApi
c_OrtApiBase_GetApi = #ptr OrtApiBase, GetApi

foreign import ccall unsafe "OrtApiBase_GetVersionString"
  c_OrtApiBase_GetVersionString :: Ptr OrtApiBase -> IO CString

getOrtApiBase :: IO OrtApiBase
getOrtApiBase = OrtApiBase <$> c_OrtGetApiBase

getOrtApi :: OrtApiBase -> OrtApi
getOrtApi = OrtApi . c_OrtApiBase_GetApi . unOrtApiBase

getVersion :: OrtApiBase -> IO String
getVersion (OrtApiBase ortApiBasePtr) = do
  versionString <- c_OrtApiBase_GetVersionString ortApiBasePtr
  peekCString versionString

-- * OrtApi

newtype OrtApi = OrtApi { unOrtApi :: Ptr OrtApi }