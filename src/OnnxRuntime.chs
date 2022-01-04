{-# LANGUAGE ForeignFunctionInterface #-}

module OnnxRuntime where

import Foreign
import Foreign.C.Types
import Foreign.C.String

#include <onnxruntime/core/session/onnxruntime_c_api.h>

-- * Configuration

{#enum ONNXTensorElementDataType {underscoreToCase} deriving (Eq, Show) #}
{#enum ONNXType {underscoreToCase} deriving (Eq, Show) #}

{#enum OrtSparseFormat {underscoreToCase} deriving (Eq, Show) #}
{#enum OrtSparseIndicesFormat {underscoreToCase} deriving (Eq, Show) #}

{#enum OrtLoggingLevel {underscoreToCase} deriving (Eq, Show) #}
{#enum OrtErrorCode {underscoreToCase} deriving (Eq, Show) #}

-- typedef struct OrtAllocator

{#enum GraphOptimizationLevel {underscoreToCase} deriving (Eq, Show) #}
{#enum ExecutionMode {underscoreToCase} deriving (Eq, Show) #}
{#enum OrtLanguageProjection {underscoreToCase} deriving (Eq, Show) #}

-- typedef struct OrtKernelInfo OrtKernelInfo;
-- typedef struct OrtKernelContext OrtKernelContext;
-- typedef struct OrtCustomOp OrtCustomOp;

{#enum OrtAllocatorType {underscoreToCase} deriving (Eq, Show) #}
{#enum OrtMemType {underscoreToCase} deriving (Eq, Show) #}
{#enum OrtCudnnConvAlgoSearch {underscoreToCase} deriving (Eq, Show) #}

-- typedef struct OrtCUDAProviderOptions
-- typedef struct OrtROCMProviderOptions
-- typedef struct OrtTensorRTProviderOptions
-- typedef struct OrtOpenVINOProviderOptions


-- * OrtApiBase

data OrtApiBase
{#pointer *OrtApiBase as OrtApiBasePtr -> OrtApiBase #}

getOrtApiBase :: IO OrtApiBasePtr
getOrtApiBase = {#call unsafe OrtGetApiBase as ^ #}

foreign import ccall "dynamic"
  mkFunOrtApiPtr :: FunPtr (CUInt -> IO (Ptr ())) -> CUInt -> IO (Ptr ())

getOrtApi :: OrtApiBasePtr -> IO OrtApiPtr
getOrtApi ortApiBasePtr = do
  getApi <- {#get OrtApiBase->GetApi #} ortApiBasePtr
  ortApiPtr <- mkFunOrtApiPtr getApi {#const ORT_API_VERSION #}
  return (castPtr ortApiPtr)

foreign import ccall "dynamic"
  mkFunCString :: FunPtr (IO CString) -> IO CString

getVersion :: OrtApiBasePtr -> IO String
getVersion ortApiBasePtr = do
  getVersionString <- {#get OrtApiBase->GetVersionString #} ortApiBasePtr
  versionCString <- mkFunCString getVersionString
  peekCString versionCString


-- * OrtApi

data OrtApi
{#pointer *OrtApi as OrtApiPtr -> OrtApi #}