{-# LANGUAGE ForeignFunctionInterface #-}

module OnnxRuntime where

import Foreign
import Foreign.C.Types
import Foreign.C.String

ortVersion :: IO String
ortVersion = do
  ortApiBase <- c_GetOrtApiBase
  versionCString <- c_GetVersionString ortApiBase
  peekCString versionCString


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

c_GetOrtApiBase :: IO OrtApiBasePtr
c_GetOrtApiBase = {#call unsafe OrtGetApiBase as ^ #}

foreign import ccall "dynamic"
  mkFunGetOrtApi :: FunPtr (CUInt -> IO (Ptr ())) -> CUInt -> IO (Ptr ())

c_GetOrtApi :: OrtApiBasePtr -> IO OrtApiPtr
c_GetOrtApi ortApiBasePtr = do
  c_GetOrtApiPtr <- {#get OrtApiBase->GetApi #} ortApiBasePtr
  ortApiPtr <- mkFunGetOrtApi c_GetOrtApiPtr {#const ORT_API_VERSION #}
  return (castPtr ortApiPtr)

foreign import ccall "dynamic"
  mkFunGetVersionString :: FunPtr (IO CString) -> IO CString

c_GetVersionString :: OrtApiBasePtr -> IO CString
c_GetVersionString ortApiBasePtr = do
  c_GetVersionStringPtr <- {#get OrtApiBase->GetVersionString #} ortApiBasePtr
  mkFunGetVersionString c_GetVersionStringPtr


-- * OrtApi

data OrtApi
{#pointer *OrtApi as OrtApiPtr -> OrtApi #}

data OrtEnv
{#pointer *OrtEnv as OrtEnvPtr -> OrtEnv #}

-- TODO: Foreign.Pool for memory allocation
-- TODO: Instance of Storable for Env and Session

-- CreateEnv
-- ReleaseEnv

-- CreateSession
-- ReleaseSession

-- Run