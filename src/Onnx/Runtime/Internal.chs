{-# LANGUAGE ForeignFunctionInterface #-}

module Onnx.Runtime.Internal where

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
{#enum GraphOptimizationLevel {underscoreToCase} deriving (Eq, Show) #}
{#enum ExecutionMode {underscoreToCase} deriving (Eq, Show) #}
{#enum OrtLanguageProjection {underscoreToCase} deriving (Eq, Show) #}
{#enum OrtAllocatorType {underscoreToCase} deriving (Eq, Show) #}
{#enum OrtMemType {underscoreToCase} deriving (Eq, Show) #}
{#enum OrtCudnnConvAlgoSearch {underscoreToCase} deriving (Eq, Show) #}

{#pointer *OrtAllocator as OrtAllocator newtype #}
-- uint32_t version;
-- void*(ORT_API_CALL* Alloc)(struct OrtAllocator* this_, size_t size);
-- void(ORT_API_CALL* Free)(struct OrtAllocator* this_, void* p);
-- const struct OrtMemoryInfo*(ORT_API_CALL* Info)(const struct OrtAllocator* this_);

{#pointer *OrtKernelInfo as OrtKernelInfo newtype #}
{#pointer *OrtKernelContext as OrtKernelContext newtype #}
{#pointer *OrtCustomOp as OrtCustomOp newtype #}

{#pointer *OrtCUDAProviderOptions as OrtCUDAProviderOptions newtype #}
-- int device_id;
-- OrtCudnnConvAlgoSearch cudnn_conv_algo_search;
-- size_t gpu_mem_limit;
-- int arena_extend_strategy;
-- int do_copy_in_default_stream;
-- int has_user_compute_stream;
-- void* user_compute_stream;
-- OrtArenaCfg* default_memory_arena_cfg;

{#pointer *OrtROCMProviderOptions as OrtROCMProviderOptions newtype #}
-- int device_id;
-- int miopen_conv_exhaustive_search;
-- size_t gpu_mem_limit;
-- int arena_extend_strategy;
-- int do_copy_in_default_stream;
-- int has_user_compute_stream;
-- void* user_compute_stream;
-- OrtArenaCfg* default_memory_arena_cfg;

{#pointer *OrtTensorRTProviderOptions as OrtTensorRTProviderOptions newtype #}
-- int device_id;
-- int has_user_compute_stream;
-- void* user_compute_stream;
-- int trt_max_partition_iterations;
-- int trt_min_subgraph_size;
-- size_t trt_max_workspace_size;
-- int trt_fp16_enable;
-- int trt_int8_enable;
-- const char* trt_int8_calibration_table_name;
-- int trt_int8_use_native_calibration_table;
-- int trt_dla_enable;
-- int trt_dla_core;
-- int trt_dump_subgraphs;
-- int trt_engine_cache_enable;
-- const char* trt_engine_cache_path;
-- int trt_engine_decryption_enable;
-- const char* trt_engine_decryption_lib_path;
-- int trt_force_sequential_engine_build;

{#pointer *OrtOpenVINOProviderOptions as OrtOpenVINOProviderOptions newtype #}
-- const char* device_type;
-- unsigned char enable_vpu_fast_compile;
-- const char* device_id;
-- size_t num_of_threads;
-- unsigned char use_compiled_network;
-- const char* blob_dump_path;
-- void* context;


-- * OrtApiBase

{#pointer *OrtApiBase as OrtApiBase newtype #}
c_GetOrtApiBase :: IO OrtApiBase
c_GetOrtApiBase = {#call unsafe OrtGetApiBase as ^ #}

foreign import ccall "dynamic"
  applyGetOrtApiPtr :: FunPtr (CUInt -> IO (Ptr ())) -> CUInt -> IO (Ptr ())

c_OrtApiBase_GetOrtApi :: OrtApiBase -> IO OrtApi
c_OrtApiBase_GetOrtApi ortApiBase = do
  c_OrtApiBase_GetOrtApiPtr <- {#get OrtApiBase->GetApi #} ortApiBase
  ortApiPtr <- applyGetOrtApiPtr c_OrtApiBase_GetOrtApiPtr {#const ORT_API_VERSION #}
  return (OrtApi $ castPtr ortApiPtr)

foreign import ccall "dynamic"
  applyGetVersionStringPtr :: FunPtr (IO CString) -> IO CString

c_OrtApiBase_GetVersionString :: OrtApiBase -> IO CString
c_OrtApiBase_GetVersionString ortApiBase = do
  c_OrtApiBase_GetVersionStringPtr <- {#get OrtApiBase->GetVersionString #} ortApiBase
  applyGetVersionStringPtr c_OrtApiBase_GetVersionStringPtr


-- * OrtApi

{#pointer *OrtApi as OrtApi newtype #}
{#pointer *OrtEnv as OrtEnv newtype #}
{#pointer *OrtStatus as OrtStatus newtype #}
{#pointer *OrtMemoryInfo as OrtMemoryInfo newtype #}
{#pointer *OrtIoBinding as OrtIoBinding newtype #}
{#pointer *OrtSession as OrtSession newtype #}
{#pointer *OrtValue as OrtValue newtype #}
{#pointer *OrtRunOptions as OrtRunOptions newtype #}
{#pointer *OrtTypeInfo as OrtTypeInfo newtype #}
{#pointer *OrtTensorTypeAndShapeInfo as OrtTensorTypeAndShapeInfo newtype #}
{#pointer *OrtSessionOptions as OrtSessionOptions newtype #}
{#pointer *OrtCustomOpDomain as OrtCustomOpDomain newtype #}
{#pointer *OrtMapTypeInfo as OrtMapTypeInfo newtype #}
{#pointer *OrtSequenceTypeInfo as OrtSequenceTypeInfo newtype #}
{#pointer *OrtModelMetadata as OrtModelMetadata newtype #}
{#pointer *OrtThreadPoolParams as OrtThreadPoolParams newtype #}
{#pointer *OrtThreadingOptions as OrtThreadingOptions newtype #}
{#pointer *OrtArenaCfg as OrtArenaCfg newtype #}
{#pointer *OrtPrepackedWeightsContainer as OrtPrepackedWeightsContainer newtype #}
{#pointer *OrtTensorRTProviderOptionsV2 as OrtTensorRTProviderOptionsV2 newtype #}


foreign import ccall "dynamic"
  applyCreateEnvPtr :: FunPtr (CInt -> CString -> Ptr OrtEnv -> IO OrtStatus) -> CInt -> CString -> Ptr OrtEnv -> IO OrtStatus

c_OrtApi_CreateEnv :: OrtApi -> OrtLoggingLevel -> CString -> Ptr OrtEnv -> IO OrtStatus
c_OrtApi_CreateEnv ortApi ortLoggingLevel logId ortEnvOut = do
  let ortLoggingLevelCInt = toEnum . fromEnum $ ortLoggingLevel
  c_OrtApi_CreateEnvPtr <- {#get OrtApi->CreateEnv #} ortApi
  applyCreateEnvPtr c_OrtApi_CreateEnvPtr ortLoggingLevelCInt logId ortEnvOut


foreign import ccall "dynamic"
  applyReleaseEnvPtr :: FunPtr (OrtEnv -> IO ()) -> OrtEnv -> IO ()

c_OrtApi_ReleaseEnv :: OrtApi -> OrtEnv -> IO ()
c_OrtApi_ReleaseEnv ortApi ortEnv = do
  c_OrtApi_ReleaseEnvPtr <- {#get OrtApi->ReleaseEnv #} ortApi
  applyReleaseEnvPtr c_OrtApi_ReleaseEnvPtr ortEnv


foreign import ccall "dynamic"
  applyCreateRunOptionsPtr :: FunPtr (Ptr OrtRunOptions -> IO OrtStatus) -> Ptr OrtRunOptions -> IO OrtStatus

c_OrtApi_CreateRunOptions :: OrtApi -> Ptr OrtRunOptions -> IO OrtStatus
c_OrtApi_CreateRunOptions ortApi ortRunOptions = do
  c_OrtApi_CreateRunOptionsPtr <- {#get OrtApi->CreateRunOptions #} ortApi
  applyCreateRunOptionsPtr c_OrtApi_CreateRunOptionsPtr ortRunOptions


foreign import ccall "dynamic"
  applyReleaseRunOptionsPtr :: FunPtr (OrtRunOptions -> IO ()) -> OrtRunOptions -> IO ()

c_OrtApi_ReleaseRunOptions :: OrtApi -> OrtRunOptions -> IO ()
c_OrtApi_ReleaseRunOptions ortApi ortRunOptions = do
  c_OrtApi_ReleaseRunOptionsPtr <- {#get OrtApi->ReleaseRunOptions #} ortApi
  applyReleaseRunOptionsPtr c_OrtApi_ReleaseRunOptionsPtr ortRunOptions


-- TODO: modelPath is wchar_t* on Windows

foreign import ccall "dynamic"
  applyCreateSessionPtr :: FunPtr (OrtEnv -> CString -> OrtSessionOptions -> Ptr OrtSession -> IO OrtStatus) -> OrtEnv -> CString -> OrtSessionOptions -> Ptr OrtSession -> IO OrtStatus

c_OrtApi_CreateSession :: OrtApi -> OrtEnv -> CString -> OrtSessionOptions -> Ptr OrtSession -> IO OrtStatus
c_OrtApi_CreateSession ortApi ortEnv modelPath ortSessionOptions ortSessionOut = do
  c_OrtApi_CreateSessionPtr <- {#get OrtApi->CreateSession #} ortApi
  applyCreateSessionPtr c_OrtApi_CreateSessionPtr ortEnv modelPath ortSessionOptions ortSessionOut


foreign import ccall "dynamic"
  applyReleaseSessionPtr :: FunPtr (OrtSession -> IO ()) -> OrtSession -> IO ()

c_OrtApi_ReleaseSession :: OrtApi -> OrtSession -> IO ()
c_OrtApi_ReleaseSession ortApi ortSession = do
  c_OrtApi_ReleaseSessionPtr <- {#get OrtApi->ReleaseSession #} ortApi
  applyReleaseSessionPtr c_OrtApi_ReleaseSessionPtr ortSession

-- c_OrtApi_Run :: OrtSession -> OrtRunOptions -> CString -> OrtValue -> CUInt -> CString -> CUInt -> Ptr OrtValue -> IO OrtStatus
-- c_OrtApi_Run ortSession ortRunOptions inputNames inputs inputLen outputNames outputNamesLen outputs = _
