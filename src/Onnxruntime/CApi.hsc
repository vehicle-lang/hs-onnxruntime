{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Onnxruntime.CApi where

import Control.Exception (Exception (..), finally, throwIO)
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe qualified as BSU
import Data.ByteString.Char8 qualified as BSC
import Data.Coerce (coerce)
import Data.Kind (Type)
import Foreign
import Foreign.C.ConstPtr.Compat (ConstPtr (..))
import Foreign.C.Types
import Foreign.C.String
import GHC.TypeLits (Natural)
import Text.Printf (printf)

#include <onnxruntime_c_api.h>

-------------------------------------------------------------------------------
-- ONNX Runtime: API Base
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- OrtApiVersion

type OrtApiVersion :: Natural
type OrtApiVersion = #const ORT_API_VERSION

type OrtApiVersionType :: Type
type OrtApiVersionType = #{type uint32_t}

{- |
The API version defined in this module.

This value is used by some API functions to behave as this version of the header expects.
-}
ortApiVersion :: OrtApiVersionType
ortApiVersion = #const ORT_API_VERSION

-------------------------------------------------------------------------------
-- OrtApiBase

{- |
The helper interface to get the right version of 'OrtApi'.

Get a pointer to this structure through 'ortGetApiBase'.
-}
newtype
  {-# CTYPE "onnxruntime_c_api.h" "OrtApiBase" #-}
  OrtApiBase = OrtApiBase { ortApiBaseConstPtr :: ConstPtr OrtApiBase }

-------------------------------------------------------------------------------
-- ortApiGetBase

{- |
The Onnxruntime library's entry point to access the C API.

Call this to get the a pointer to an 'OrtApiBase'.
-}
ortGetApiBase :: IO OrtApiBase
ortGetApiBase = coerce _wrap_ortGetApiBase
{-# INLINE ortGetApiBase #-}

foreign import capi unsafe
  "onnxruntime_c_api.h OrtGetApiBase"
  _wrap_ortGetApiBase ::
    IO (ConstPtr OrtApiBase)

-------------------------------------------------------------------------------
-- OrtApiBase::GetVersionString

{- |
Returns a null terminated string of the version of the Onnxruntime library (eg: "1.8.1").
-}
ortApiBaseGetVersionString ::
  OrtApiBase ->
  IO String
ortApiBaseGetVersionString ortApiBase = do
  ConstPtr versionStringPtr <- _wrap_OrtApiBase_GetVersionString ortApiBase.ortApiBaseConstPtr
  peekCString versionStringPtr

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_OrtApiBase_GetVersionString"
  _wrap_OrtApiBase_GetVersionString ::
    ConstPtr OrtApiBase ->
    IO (ConstPtr CChar)

#{def
  const char* _wrap_OrtApiBase_GetVersionString(const OrtApiBase* ortApiBase) {
    return ortApiBase->GetVersionString();
  }
}

-------------------------------------------------------------------------------
-- OrtApiBase::GetApi

{- |
Get a pointer to the requested version of the 'OrtApi'
-}
ortApiBaseGetApi ::
  OrtApiBase ->
  OrtApiVersionType ->
  IO OrtApi
ortApiBaseGetApi = coerce _wrap_OrtApiBase_GetApi
{-# INLINE ortApiBaseGetApi #-}

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_OrtApiBase_GetApi"
  _wrap_OrtApiBase_GetApi ::
    ConstPtr OrtApiBase ->
    OrtApiVersionType ->
    IO (ConstPtr OrtApi)

#{def
  const OrtApi* _wrap_OrtApiBase_GetApi(const OrtApiBase* ortApiBase, uint32_t version) {
    return ortApiBase->GetApi(version);
  }
}


-------------------------------------------------------------------------------
-- ONNX Runtime: Primitive Types
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- OrtLoggingLevel

{-|
> typedef enum OrtLoggingLevel {
>   ORT_LOGGING_LEVEL_VERBOSE,  ///< Verbose informational messages (least severe).
>   ORT_LOGGING_LEVEL_INFO,     ///< Informational messages.
>   ORT_LOGGING_LEVEL_WARNING,  ///< Warning messages.
>   ORT_LOGGING_LEVEL_ERROR,    ///< Error messages.
>   ORT_LOGGING_LEVEL_FATAL,    ///< Fatal error messages (most severe).
> } OrtLoggingLevel;
-}
newtype
  {-# CTYPE "onnxruntime_c_api.h" "OrtLoggingLevel" #-}
  OrtLoggingLevel = OrtLoggingLevel
    { unOrtLoggingLevel :: #{type OrtLoggingLevel}
    }
    deriving (Eq, Show)

pattern OrtLoggingLevelVerbose :: OrtLoggingLevel
pattern OrtLoggingLevelVerbose = OrtLoggingLevel ( #{const ORT_LOGGING_LEVEL_VERBOSE} )

pattern OrtLoggingLevelInfo :: OrtLoggingLevel
pattern OrtLoggingLevelInfo = OrtLoggingLevel ( #{const ORT_LOGGING_LEVEL_INFO} )

pattern OrtLoggingLevelWarning :: OrtLoggingLevel
pattern OrtLoggingLevelWarning = OrtLoggingLevel ( #{const ORT_LOGGING_LEVEL_WARNING} )

pattern OrtLoggingLevelError :: OrtLoggingLevel
pattern OrtLoggingLevelError = OrtLoggingLevel ( #{const ORT_LOGGING_LEVEL_ERROR} )

pattern OrtLoggingLevelFatal :: OrtLoggingLevel
pattern OrtLoggingLevelFatal = OrtLoggingLevel ( #{const ORT_LOGGING_LEVEL_FATAL} )

{-# COMPLETE
  OrtLoggingLevelVerbose,
  OrtLoggingLevelInfo,
  OrtLoggingLevelWarning,
  OrtLoggingLevelError,
  OrtLoggingLevelFatal
  #-}

-------------------------------------------------------------------------------
-- GraphOptimizationLevel

{- |
> typedef enum GraphOptimizationLevel {
>   ORT_DISABLE_ALL = 0,
>   ORT_ENABLE_BASIC = 1,
>   ORT_ENABLE_EXTENDED = 2,
>   ORT_ENABLE_ALL = 99
> } GraphOptimizationLevel;
-}
newtype
  {-# CTYPE "onnxruntime_c_api.h" "GraphOptimizationLevel" #-}
  GraphOptimizationLevel = GraphOptimizationLevel
    { unGraphOptimizationLevel :: #{type GraphOptimizationLevel}
    }
    deriving (Eq, Show)


pattern OrtDisableAll :: GraphOptimizationLevel
pattern OrtDisableAll = GraphOptimizationLevel ( #{const ORT_DISABLE_ALL} )

pattern OrtEnableBasic :: GraphOptimizationLevel
pattern OrtEnableBasic = GraphOptimizationLevel ( #{const ORT_ENABLE_BASIC} )

pattern OrtEnableExtended :: GraphOptimizationLevel
pattern OrtEnableExtended = GraphOptimizationLevel ( #{const ORT_ENABLE_EXTENDED} )

pattern OrtEnableAll :: GraphOptimizationLevel
pattern OrtEnableAll = GraphOptimizationLevel ( #{const ORT_ENABLE_ALL} )

{-# COMPLETE
  OrtDisableAll,
  OrtEnableBasic,
  OrtEnableExtended,
  OrtEnableAll
  #-}

-------------------------------------------------------------------------------
-- ExecutionMode

{- |
> typedef enum ExecutionMode {
>   ORT_SEQUENTIAL = 0,
>   ORT_PARALLEL = 1,
> } ExecutionMode;
-}
newtype
  {-# CTYPE "onnxruntime_c_api.h" "ExecutionMode" #-}
  ExecutionMode = ExecutionMode
    { unExecutionMode :: #{type ExecutionMode}
    }
    deriving (Eq, Show)

pattern OrtSequential :: ExecutionMode
pattern OrtSequential = ExecutionMode ( #{const ORT_SEQUENTIAL} )

pattern OrtParallel :: ExecutionMode
pattern OrtParallel = ExecutionMode ( #{const ORT_PARALLEL} )

{-# COMPLETE
  OrtSequential,
  OrtParallel
  #-}

-------------------------------------------------------------------------------
-- ONNXTensorElementDataType

{- |
> typedef enum ONNXTensorElementDataType {
>   ONNX_TENSOR_ELEMENT_DATA_TYPE_UNDEFINED,
>   ONNX_TENSOR_ELEMENT_DATA_TYPE_FLOAT,
>   ONNX_TENSOR_ELEMENT_DATA_TYPE_UINT8,
>   ONNX_TENSOR_ELEMENT_DATA_TYPE_INT8,
>   ONNX_TENSOR_ELEMENT_DATA_TYPE_UINT16,
>   ONNX_TENSOR_ELEMENT_DATA_TYPE_INT16,
>   ONNX_TENSOR_ELEMENT_DATA_TYPE_INT32,
>   ONNX_TENSOR_ELEMENT_DATA_TYPE_INT64,
>   ONNX_TENSOR_ELEMENT_DATA_TYPE_STRING,
>   ONNX_TENSOR_ELEMENT_DATA_TYPE_BOOL,
>   ONNX_TENSOR_ELEMENT_DATA_TYPE_FLOAT16,
>   ONNX_TENSOR_ELEMENT_DATA_TYPE_DOUBLE,
>   ONNX_TENSOR_ELEMENT_DATA_TYPE_UINT32,
>   ONNX_TENSOR_ELEMENT_DATA_TYPE_UINT64,
>   ONNX_TENSOR_ELEMENT_DATA_TYPE_COMPLEX64,
>   ONNX_TENSOR_ELEMENT_DATA_TYPE_COMPLEX128,
>   ONNX_TENSOR_ELEMENT_DATA_TYPE_BFLOAT16,
>   ONNX_TENSOR_ELEMENT_DATA_TYPE_FLOAT8E4M3FN,
>   ONNX_TENSOR_ELEMENT_DATA_TYPE_FLOAT8E4M3FNUZ,
>   ONNX_TENSOR_ELEMENT_DATA_TYPE_FLOAT8E5M2,
>   ONNX_TENSOR_ELEMENT_DATA_TYPE_FLOAT8E5M2FNUZ,
>   ONNX_TENSOR_ELEMENT_DATA_TYPE_UINT4,
>   ONNX_TENSOR_ELEMENT_DATA_TYPE_INT4
> } ONNXTensorElementDataType;
-}
newtype
  {-# CTYPE "onnxruntime_c_api.h" "ONNXTensorElementDataType" #-}
  ONNXTensorElementDataType = ONNXTensorElementDataType
    { unONNXTensorElementDataType :: #{type ONNXTensorElementDataType}
    }
    deriving (Eq, Show)

pattern OnnxTensorElementDataTypeUndefined :: ONNXTensorElementDataType
pattern OnnxTensorElementDataTypeUndefined = ONNXTensorElementDataType ( #{const ONNX_TENSOR_ELEMENT_DATA_TYPE_UNDEFINED} )

pattern OnnxTensorElementDataTypeFloat :: ONNXTensorElementDataType
pattern OnnxTensorElementDataTypeFloat = ONNXTensorElementDataType ( #{const ONNX_TENSOR_ELEMENT_DATA_TYPE_FLOAT} )

pattern OnnxTensorElementDataTypeUint8 :: ONNXTensorElementDataType
pattern OnnxTensorElementDataTypeUint8 = ONNXTensorElementDataType ( #{const ONNX_TENSOR_ELEMENT_DATA_TYPE_UINT8} )

pattern OnnxTensorElementDataTypeInt8 :: ONNXTensorElementDataType
pattern OnnxTensorElementDataTypeInt8 = ONNXTensorElementDataType ( #{const ONNX_TENSOR_ELEMENT_DATA_TYPE_INT8} )

pattern OnnxTensorElementDataTypeUint16 :: ONNXTensorElementDataType
pattern OnnxTensorElementDataTypeUint16 = ONNXTensorElementDataType ( #{const ONNX_TENSOR_ELEMENT_DATA_TYPE_UINT16} )

pattern OnnxTensorElementDataTypeInt16 :: ONNXTensorElementDataType
pattern OnnxTensorElementDataTypeInt16 = ONNXTensorElementDataType ( #{const ONNX_TENSOR_ELEMENT_DATA_TYPE_INT16} )

pattern OnnxTensorElementDataTypeInt32 :: ONNXTensorElementDataType
pattern OnnxTensorElementDataTypeInt32 = ONNXTensorElementDataType ( #{const ONNX_TENSOR_ELEMENT_DATA_TYPE_INT32} )

pattern OnnxTensorElementDataTypeInt64 :: ONNXTensorElementDataType
pattern OnnxTensorElementDataTypeInt64 = ONNXTensorElementDataType ( #{const ONNX_TENSOR_ELEMENT_DATA_TYPE_INT64} )

pattern OnnxTensorElementDataTypeString :: ONNXTensorElementDataType
pattern OnnxTensorElementDataTypeString = ONNXTensorElementDataType ( #{const ONNX_TENSOR_ELEMENT_DATA_TYPE_STRING} )

pattern OnnxTensorElementDataTypeBool :: ONNXTensorElementDataType
pattern OnnxTensorElementDataTypeBool = ONNXTensorElementDataType ( #{const ONNX_TENSOR_ELEMENT_DATA_TYPE_BOOL} )

pattern OnnxTensorElementDataTypeFloat16 :: ONNXTensorElementDataType
pattern OnnxTensorElementDataTypeFloat16 = ONNXTensorElementDataType ( #{const ONNX_TENSOR_ELEMENT_DATA_TYPE_FLOAT16} )

pattern OnnxTensorElementDataTypeDouble :: ONNXTensorElementDataType
pattern OnnxTensorElementDataTypeDouble = ONNXTensorElementDataType ( #{const ONNX_TENSOR_ELEMENT_DATA_TYPE_DOUBLE} )

pattern OnnxTensorElementDataTypeUint32 :: ONNXTensorElementDataType
pattern OnnxTensorElementDataTypeUint32 = ONNXTensorElementDataType ( #{const ONNX_TENSOR_ELEMENT_DATA_TYPE_UINT32} )

pattern OnnxTensorElementDataTypeUint64 :: ONNXTensorElementDataType
pattern OnnxTensorElementDataTypeUint64 = ONNXTensorElementDataType ( #{const ONNX_TENSOR_ELEMENT_DATA_TYPE_UINT64} )

pattern OnnxTensorElementDataTypeComplex64 :: ONNXTensorElementDataType
pattern OnnxTensorElementDataTypeComplex64 = ONNXTensorElementDataType ( #{const ONNX_TENSOR_ELEMENT_DATA_TYPE_COMPLEX64} )

pattern OnnxTensorElementDataTypeComplex128 :: ONNXTensorElementDataType
pattern OnnxTensorElementDataTypeComplex128 = ONNXTensorElementDataType ( #{const ONNX_TENSOR_ELEMENT_DATA_TYPE_COMPLEX128} )

pattern OnnxTensorElementDataTypeBfloat16 :: ONNXTensorElementDataType
pattern OnnxTensorElementDataTypeBfloat16 = ONNXTensorElementDataType ( #{const ONNX_TENSOR_ELEMENT_DATA_TYPE_BFLOAT16} )

pattern OnnxTensorElementDataTypeFloat8e4m3fn :: ONNXTensorElementDataType
pattern OnnxTensorElementDataTypeFloat8e4m3fn = ONNXTensorElementDataType ( #{const ONNX_TENSOR_ELEMENT_DATA_TYPE_FLOAT8E4M3FN} )

pattern OnnxTensorElementDataTypeFloat8e4m3fnuz :: ONNXTensorElementDataType
pattern OnnxTensorElementDataTypeFloat8e4m3fnuz = ONNXTensorElementDataType ( #{const ONNX_TENSOR_ELEMENT_DATA_TYPE_FLOAT8E4M3FNUZ} )

pattern OnnxTensorElementDataTypeFloat8e5m2 :: ONNXTensorElementDataType
pattern OnnxTensorElementDataTypeFloat8e5m2 = ONNXTensorElementDataType ( #{const ONNX_TENSOR_ELEMENT_DATA_TYPE_FLOAT8E5M2} )

pattern OnnxTensorElementDataTypeFloat8e5m2fnuz :: ONNXTensorElementDataType
pattern OnnxTensorElementDataTypeFloat8e5m2fnuz = ONNXTensorElementDataType ( #{const ONNX_TENSOR_ELEMENT_DATA_TYPE_FLOAT8E5M2FNUZ} )

pattern OnnxTensorElementDataTypeUint4 :: ONNXTensorElementDataType
pattern OnnxTensorElementDataTypeUint4 = ONNXTensorElementDataType ( #{const ONNX_TENSOR_ELEMENT_DATA_TYPE_UINT4} )

pattern OnnxTensorElementDataTypeInt4 :: ONNXTensorElementDataType
pattern OnnxTensorElementDataTypeInt4 = ONNXTensorElementDataType ( #{const ONNX_TENSOR_ELEMENT_DATA_TYPE_INT4} )

{-# COMPLETE
  OnnxTensorElementDataTypeUndefined,
  OnnxTensorElementDataTypeFloat,
  OnnxTensorElementDataTypeUint8,
  OnnxTensorElementDataTypeInt8,
  OnnxTensorElementDataTypeUint16,
  OnnxTensorElementDataTypeInt16,
  OnnxTensorElementDataTypeInt32,
  OnnxTensorElementDataTypeInt64,
  OnnxTensorElementDataTypeString,
  OnnxTensorElementDataTypeBool,
  OnnxTensorElementDataTypeFloat16,
  OnnxTensorElementDataTypeDouble,
  OnnxTensorElementDataTypeUint32,
  OnnxTensorElementDataTypeUint64,
  OnnxTensorElementDataTypeComplex64,
  OnnxTensorElementDataTypeComplex128,
  OnnxTensorElementDataTypeBfloat16,
  OnnxTensorElementDataTypeFloat8e4m3fn,
  OnnxTensorElementDataTypeFloat8e4m3fnuz,
  OnnxTensorElementDataTypeFloat8e5m2,
  OnnxTensorElementDataTypeFloat8e5m2fnuz,
  OnnxTensorElementDataTypeUint4,
  OnnxTensorElementDataTypeInt4
  #-}

-------------------------------------------------------------------------------
-- ONNXType

{-
> typedef enum ONNXType {
>   ONNX_TYPE_UNKNOWN,
>   ONNX_TYPE_TENSOR,
>   ONNX_TYPE_SEQUENCE,
>   ONNX_TYPE_MAP,
>   ONNX_TYPE_OPAQUE,
>   ONNX_TYPE_SPARSETENSOR,
>   ONNX_TYPE_OPTIONAL
> } ONNXType;
-}

-------------------------------------------------------------------------------
-- OrtErrorCode

{- |
> typedef enum OrtErrorCode {
>   ORT_OK,
>   ORT_FAIL,
>   ORT_INVALID_ARGUMENT,
>   ORT_NO_SUCHFILE,
>   ORT_NO_MODEL,
>   ORT_ENGINE_ERROR,
>   ORT_RUNTIME_EXCEPTION,
>   ORT_INVALID_PROTOBUF,
>   ORT_MODEL_LOADED,
>   ORT_NOT_IMPLEMENTED,
>   ORT_INVALID_GRAPH,
>   ORT_EP_FAIL,
> } OrtErrorCode;
-}
newtype
  {-# CTYPE "onnxruntime_c_api.h" "OrtErrorCode" #-}
  OrtErrorCode = OrtErrorCode
    { unOrtErrorCode :: #{type OrtErrorCode}
    }
    deriving (Eq, Show)

pattern OrtOk :: OrtErrorCode
pattern OrtOk = OrtErrorCode ( #{const ORT_OK} )

pattern OrtFail :: OrtErrorCode
pattern OrtFail = OrtErrorCode ( #{const ORT_FAIL} )

pattern OrtInvalidArgument :: OrtErrorCode
pattern OrtInvalidArgument = OrtErrorCode ( #{const ORT_INVALID_ARGUMENT} )

pattern OrtNoSuchfile :: OrtErrorCode
pattern OrtNoSuchfile = OrtErrorCode ( #{const ORT_NO_SUCHFILE} )

pattern OrtNoModel :: OrtErrorCode
pattern OrtNoModel = OrtErrorCode ( #{const ORT_NO_MODEL} )

pattern OrtEngineError :: OrtErrorCode
pattern OrtEngineError = OrtErrorCode ( #{const ORT_ENGINE_ERROR} )

pattern OrtRuntimeException :: OrtErrorCode
pattern OrtRuntimeException = OrtErrorCode ( #{const ORT_RUNTIME_EXCEPTION} )

pattern OrtInvalidProtobuf :: OrtErrorCode
pattern OrtInvalidProtobuf = OrtErrorCode ( #{const ORT_INVALID_PROTOBUF} )

pattern OrtModelLoaded :: OrtErrorCode
pattern OrtModelLoaded = OrtErrorCode ( #{const ORT_MODEL_LOADED} )

pattern OrtNotImplemented :: OrtErrorCode
pattern OrtNotImplemented = OrtErrorCode ( #{const ORT_NOT_IMPLEMENTED} )

pattern OrtInvalidGraph :: OrtErrorCode
pattern OrtInvalidGraph = OrtErrorCode ( #{const ORT_INVALID_GRAPH} )

pattern OrtEpFail :: OrtErrorCode
pattern OrtEpFail = OrtErrorCode ( #{const ORT_EP_FAIL} )

{-# COMPLETE
  OrtOk,
  OrtFail,
  OrtInvalidArgument,
  OrtNoSuchfile,
  OrtNoModel,
  OrtEngineError,
  OrtRuntimeException,
  OrtInvalidProtobuf,
  OrtModelLoaded,
  OrtNotImplemented,
  OrtInvalidGraph,
  OrtEpFail
  #-}

-------------------------------------------------------------------------------
-- ONNX Runtime: Types
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- OrtApi

newtype
  {-# CTYPE "onnxruntime_c_api.h" "OrtApi" #-}
  OrtApi = OrtApi { ortApiConstPtr :: ConstPtr OrtApi }

class HasOrtApi a where
  getOrtApi :: a -> IO OrtApi

-------------------------------------------------------------------------------
-- OrtAllocator

data
  {-# CTYPE "onnxruntime_c_api.h" "COrtAllocator" #-}
  COrtAllocator

#{def
  typedef OrtAllocator COrtAllocator;
}

newtype
  {-# CTYPE "Onnxruntime/CApi_hsc.h" "HsOrtAllocator" #-}
  OrtAllocator = OrtAllocator { ortAllocatorForeignPtr :: ForeignPtr OrtAllocator }

#{def
  typedef struct HsOrtAllocator {
    const OrtApi* ortApi;
    COrtAllocator* ortAllocator;
  } HsOrtAllocator;
}

instance HasOrtApi OrtAllocator where
  getOrtApi ortAllocator =
    withOrtAllocatorPtr ortAllocator $ \ortAllocatorPtr ->
      OrtApi <$> #{peek HsOrtAllocator, ortApi} ortAllocatorPtr

-- | Internal helper.
withOrtAllocatorPtr ::
  OrtAllocator ->
  (Ptr OrtAllocator -> IO a) ->
  IO a
withOrtAllocatorPtr ortAllocator =
  withForeignPtr ortAllocator.ortAllocatorForeignPtr

-- | Internal helper.
withCOrtAllocatorPtr ::
  OrtAllocator ->
  (Ptr COrtAllocator -> IO a) ->
  IO a
withCOrtAllocatorPtr ortAllocator action =
  withOrtAllocatorPtr ortAllocator $ \ortAllocatorPtr -> do
    cOrtAllocatorPtr <- #{peek HsOrtAllocator, ortAllocator} ortAllocatorPtr
    action cOrtAllocatorPtr

-- | Internal helper.
wrapCOrtAllocator ::
  OrtApi ->
  Ptr COrtAllocator ->
  IO OrtAllocator
wrapCOrtAllocator ortApi rawOrtAllocatorPtr = do
  ortAllocatorPtr <- _wrap_COrtAllocator ortApi.ortApiConstPtr rawOrtAllocatorPtr
  ortAllocatorForeignPtr <- newForeignPtr _wrap_OrtApi_ReleaseAllocator ortAllocatorPtr
  pure $ OrtAllocator ortAllocatorForeignPtr

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_COrtAllocator"
  _wrap_COrtAllocator ::
    ConstPtr OrtApi ->
    Ptr COrtAllocator ->
    IO (Ptr OrtAllocator)

#{def
  HsOrtAllocator* _wrap_COrtAllocator(
    const OrtApi* ortApi,
    COrtAllocator* ortAllocator
  ) {
    HsOrtAllocator *out = malloc(sizeof *out);
    out->ortApi = ortApi;
    out->ortAllocator = ortAllocator;
    return out;
  }
}

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h &_wrap_OrtApi_ReleaseAllocator"
  _wrap_OrtApi_ReleaseAllocator ::
    FunPtr (
      Ptr OrtAllocator ->
      IO ()
    )

#{def
  void _wrap_OrtApi_ReleaseAllocator(HsOrtAllocator* ortAllocator) {
    ortAllocator->ortApi->ReleaseAllocator(ortAllocator->ortAllocator);
    free(ortAllocator);
  }
}

-------------------------------------------------------------------------------
-- OrtEnv

data
  {-# CTYPE "onnxruntime_c_api.h" "COrtEnv" #-}
  COrtEnv

#{def
  typedef OrtEnv COrtEnv;
}

newtype
  {-# CTYPE "Onnxruntime/CApi_hsc.h" "HsOrtEnv" #-}
  OrtEnv = OrtEnv { ortEnvForeignPtr :: ForeignPtr OrtEnv }

#{def
  typedef struct HsOrtEnv {
    const OrtApi* ortApi;
    COrtEnv* ortEnv;
  } HsOrtEnv;
}

instance HasOrtApi OrtEnv where
  getOrtApi ortEnv =
    withOrtEnvPtr ortEnv $ \ortEnvPtr ->
      OrtApi <$> #{peek HsOrtEnv, ortApi} ortEnvPtr

-- | Internal helper.
withOrtEnvPtr ::
  OrtEnv ->
  (Ptr OrtEnv -> IO a) ->
  IO a
withOrtEnvPtr ortEnv =
  withForeignPtr ortEnv.ortEnvForeignPtr

-- | Internal helper.
withCOrtEnvPtr ::
  OrtEnv ->
  (Ptr COrtEnv -> IO a) ->
  IO a
withCOrtEnvPtr ortEnv action =
  withOrtEnvPtr ortEnv $ \ortEnvPtr -> do
    cOrtEnvPtr <- #{peek HsOrtEnv, ortEnv} ortEnvPtr
    action cOrtEnvPtr

-- | Internal helper.
wrapCOrtEnv ::
  OrtApi ->
  Ptr COrtEnv ->
  IO OrtEnv
wrapCOrtEnv ortApi rawOrtEnvPtr = do
  ortEnvPtr <- _wrap_COrtEnv ortApi.ortApiConstPtr rawOrtEnvPtr
  ortEnvForeignPtr <- newForeignPtr _wrap_OrtApi_ReleaseEnv ortEnvPtr
  pure $ OrtEnv ortEnvForeignPtr

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_COrtEnv"
  _wrap_COrtEnv ::
    ConstPtr OrtApi ->
    Ptr COrtEnv ->
    IO (Ptr OrtEnv)

#{def
  HsOrtEnv* _wrap_COrtEnv(
    const OrtApi* ortApi,
    COrtEnv* ortEnv
  ) {
    HsOrtEnv *out = malloc(sizeof *out);
    out->ortApi = ortApi;
    out->ortEnv = ortEnv;
    return out;
  }
}

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h &_wrap_OrtApi_ReleaseEnv"
  _wrap_OrtApi_ReleaseEnv ::
    FunPtr (
      Ptr OrtEnv ->
      IO ()
    )

#{def
  void _wrap_OrtApi_ReleaseEnv(HsOrtEnv* ortEnv) {
    ortEnv->ortApi->ReleaseEnv(ortEnv->ortEnv);
    free(ortEnv);
  }
}

-------------------------------------------------------------------------------
-- OrtMapTypeInfo

data
  {-# CTYPE "onnxruntime_c_api.h" "COrtMapTypeInfo" #-}
  COrtMapTypeInfo

#{def
  typedef OrtMapTypeInfo COrtMapTypeInfo;
}

newtype
  {-# CTYPE "Onnxruntime/CApi_hsc.h" "HsOrtMapTypeInfo" #-}
  OrtMapTypeInfo = OrtMapTypeInfo { ortMapTypeInfoForeignPtr :: ForeignPtr OrtMapTypeInfo }

#{def
  typedef struct HsOrtMapTypeInfo {
    const OrtApi* ortApi;
    COrtMapTypeInfo* ortMapTypeInfo;
  } HsOrtMapTypeInfo;
}

instance HasOrtApi OrtMapTypeInfo where
  getOrtApi ortMapTypeInfo =
    withOrtMapTypeInfoPtr ortMapTypeInfo $ \ortMapTypeInfoPtr ->
      OrtApi <$> #{peek HsOrtMapTypeInfo, ortApi} ortMapTypeInfoPtr

-- | Internal helper.
withOrtMapTypeInfoPtr ::
  OrtMapTypeInfo ->
  (Ptr OrtMapTypeInfo -> IO a) ->
  IO a
withOrtMapTypeInfoPtr ortMapTypeInfo =
  withForeignPtr ortMapTypeInfo.ortMapTypeInfoForeignPtr

-- | Internal helper.
wrapCOrtMapTypeInfo ::
  OrtApi ->
  Ptr COrtMapTypeInfo ->
  IO OrtMapTypeInfo
wrapCOrtMapTypeInfo ortApi rawOrtMapTypeInfoPtr = do
  ortMapTypeInfoPtr <- _wrap_COrtMapTypeInfo ortApi.ortApiConstPtr rawOrtMapTypeInfoPtr
  ortMapTypeInfoForeignPtr <- newForeignPtr _wrap_OrtApi_ReleaseMapTypeInfo ortMapTypeInfoPtr
  pure $ OrtMapTypeInfo ortMapTypeInfoForeignPtr

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_COrtMapTypeInfo"
  _wrap_COrtMapTypeInfo ::
    ConstPtr OrtApi ->
    Ptr COrtMapTypeInfo ->
    IO (Ptr OrtMapTypeInfo)

#{def
  HsOrtMapTypeInfo* _wrap_COrtMapTypeInfo(
    const OrtApi* ortApi,
    COrtMapTypeInfo* ortMapTypeInfo
  ) {
    HsOrtMapTypeInfo *out = malloc(sizeof *out);
    out->ortApi = ortApi;
    out->ortMapTypeInfo = ortMapTypeInfo;
    return out;
  }
}

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h &_wrap_OrtApi_ReleaseMapTypeInfo"
  _wrap_OrtApi_ReleaseMapTypeInfo ::
    FunPtr (
      Ptr OrtMapTypeInfo ->
      IO ()
    )

#{def
  void _wrap_OrtApi_ReleaseMapTypeInfo(HsOrtMapTypeInfo* ortMapTypeInfo) {
    ortMapTypeInfo->ortApi->ReleaseMapTypeInfo(ortMapTypeInfo->ortMapTypeInfo);
    free(ortMapTypeInfo);
  }
}

-------------------------------------------------------------------------------
-- OrtMemoryInfo

data
  {-# CTYPE "onnxruntime_c_api.h" "COrtMemoryInfo" #-}
  COrtMemoryInfo

#{def
  typedef OrtMemoryInfo COrtMemoryInfo;
}

newtype
  {-# CTYPE "Onnxruntime/CApi_hsc.h" "HsOrtMemoryInfo" #-}
  OrtMemoryInfo = OrtMemoryInfo { ortMemoryInfoForeignPtr :: ForeignPtr OrtMemoryInfo }

#{def
  typedef struct HsOrtMemoryInfo {
    const OrtApi* ortApi;
    COrtMemoryInfo* ortMemoryInfo;
  } HsOrtMemoryInfo;
}

instance HasOrtApi OrtMemoryInfo where
  getOrtApi ortMemoryInfo =
    withOrtMemoryInfoPtr ortMemoryInfo $ \ortMemoryInfoPtr ->
      OrtApi <$> #{peek HsOrtMemoryInfo, ortApi} ortMemoryInfoPtr

-- | Internal helper.
withOrtMemoryInfoPtr ::
  OrtMemoryInfo ->
  (Ptr OrtMemoryInfo -> IO a) ->
  IO a
withOrtMemoryInfoPtr ortMemoryInfo =
  withForeignPtr ortMemoryInfo.ortMemoryInfoForeignPtr

-- | Internal helper.
wrapCOrtMemoryInfo ::
  OrtApi ->
  Ptr COrtMemoryInfo ->
  IO OrtMemoryInfo
wrapCOrtMemoryInfo ortApi rawOrtMemoryInfoPtr = do
  ortMemoryInfoPtr <- _wrap_COrtMemoryInfo ortApi.ortApiConstPtr rawOrtMemoryInfoPtr
  ortMemoryInfoForeignPtr <- newForeignPtr _wrap_OrtApi_ReleaseMemoryInfo ortMemoryInfoPtr
  pure $ OrtMemoryInfo ortMemoryInfoForeignPtr

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_COrtMemoryInfo"
  _wrap_COrtMemoryInfo ::
    ConstPtr OrtApi ->
    Ptr COrtMemoryInfo ->
    IO (Ptr OrtMemoryInfo)

#{def
  HsOrtMemoryInfo* _wrap_COrtMemoryInfo(
    const OrtApi* ortApi,
    COrtMemoryInfo* ortMemoryInfo
  ) {
    HsOrtMemoryInfo *out = malloc(sizeof *out);
    out->ortApi = ortApi;
    out->ortMemoryInfo = ortMemoryInfo;
    return out;
  }
}

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h &_wrap_OrtApi_ReleaseMemoryInfo"
  _wrap_OrtApi_ReleaseMemoryInfo ::
    FunPtr (
      Ptr OrtMemoryInfo ->
      IO ()
    )

#{def
  void _wrap_OrtApi_ReleaseMemoryInfo(HsOrtMemoryInfo* ortMemoryInfo) {
    ortMemoryInfo->ortApi->ReleaseMemoryInfo(ortMemoryInfo->ortMemoryInfo);
    free(ortMemoryInfo);
  }
}

-------------------------------------------------------------------------------
-- OrtSession

data
  {-# CTYPE "onnxruntime_c_api.h" "COrtSession" #-}
  COrtSession

#{def
  typedef OrtSession COrtSession;
}

newtype
  {-# CTYPE "Onnxruntime/CApi_hsc.h" "HsOrtSession" #-}
  OrtSession = OrtSession { ortSessionForeignPtr :: ForeignPtr OrtSession }

#{def
  typedef struct HsOrtSession {
    const OrtApi* ortApi;
    COrtSession* ortSession;
  } HsOrtSession;
}

instance HasOrtApi OrtSession where
  getOrtApi ortSession =
    withOrtSessionPtr ortSession $ \ortSessionPtr ->
      OrtApi <$> #{peek HsOrtSession, ortApi} ortSessionPtr

-- | Internal helper.
withOrtSessionPtr ::
  OrtSession ->
  (Ptr OrtSession -> IO a) ->
  IO a
withOrtSessionPtr ortSession =
  withForeignPtr ortSession.ortSessionForeignPtr

-- | Internal helper.
wrapCOrtSession ::
  OrtApi ->
  Ptr COrtSession ->
  IO OrtSession
wrapCOrtSession ortApi rawOrtSessionPtr = do
  ortSessionPtr <- _wrap_COrtSession ortApi.ortApiConstPtr rawOrtSessionPtr
  ortSessionForeignPtr <- newForeignPtr _wrap_OrtApi_ReleaseSession ortSessionPtr
  pure $ OrtSession ortSessionForeignPtr

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_COrtSession"
  _wrap_COrtSession ::
    ConstPtr OrtApi ->
    Ptr COrtSession ->
    IO (Ptr OrtSession)

#{def
  HsOrtSession* _wrap_COrtSession(
    const OrtApi* ortApi,
    COrtSession* ortSession
  ) {
    HsOrtSession *out = malloc(sizeof *out);
    out->ortApi = ortApi;
    out->ortSession = ortSession;
    return out;
  }
}

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h &_wrap_OrtApi_ReleaseSession"
  _wrap_OrtApi_ReleaseSession ::
    FunPtr (
      Ptr OrtSession ->
      IO ()
    )

#{def
  void _wrap_OrtApi_ReleaseSession(HsOrtSession* ortSession) {
    ortSession->ortApi->ReleaseSession(ortSession->ortSession);
    free(ortSession);
  }
}

-------------------------------------------------------------------------------
-- OrtSessionOptions

data
  {-# CTYPE "onnxruntime_c_api.h" "COrtSessionOptions" #-}
  COrtSessionOptions

#{def
  typedef OrtSessionOptions COrtSessionOptions;
}

newtype
  {-# CTYPE "Onnxruntime/CApi_hsc.h" "HsOrtSessionOptions" #-}
  OrtSessionOptions = OrtSessionOptions { ortSessionOptionsForeignPtr :: ForeignPtr OrtSessionOptions }

#{def
  typedef struct HsOrtSessionOptions {
    const OrtApi* ortApi;
    COrtSessionOptions* ortSessionOptions;
  } HsOrtSessionOptions;
}

instance HasOrtApi OrtSessionOptions where
  getOrtApi ortSessionOptions =
    withOrtSessionOptionsPtr ortSessionOptions $ \ortSessionOptionsPtr ->
      OrtApi <$> #{peek HsOrtSessionOptions, ortApi} ortSessionOptionsPtr

-- | Internal helper.
withOrtSessionOptionsPtr ::
  OrtSessionOptions ->
  (Ptr OrtSessionOptions -> IO a) ->
  IO a
withOrtSessionOptionsPtr ortSessionOptions =
  withForeignPtr ortSessionOptions.ortSessionOptionsForeignPtr

-- | Internal helper.
withCOrtSessionOptionsPtr ::
  OrtSessionOptions ->
  (Ptr COrtSessionOptions -> IO a) ->
  IO a
withCOrtSessionOptionsPtr ortSessionOptions action =
  withOrtSessionOptionsPtr ortSessionOptions $ \ortSessionOptionsPtr -> do
    cOrtSessionOptionsPtr <- #{peek HsOrtSessionOptions, ortSessionOptions} ortSessionOptionsPtr
    action cOrtSessionOptionsPtr

-- | Internal helper.
wrapCOrtSessionOptions ::
  OrtApi ->
  Ptr COrtSessionOptions ->
  IO OrtSessionOptions
wrapCOrtSessionOptions ortApi rawOrtSessionOptionsPtr = do
  ortSessionOptionsPtr <- _wrap_COrtSessionOptions ortApi.ortApiConstPtr rawOrtSessionOptionsPtr
  ortSessionOptionsForeignPtr <- newForeignPtr _wrap_OrtApi_ReleaseSessionOptions ortSessionOptionsPtr
  pure $ OrtSessionOptions ortSessionOptionsForeignPtr

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_COrtSessionOptions"
  _wrap_COrtSessionOptions ::
    ConstPtr OrtApi ->
    Ptr COrtSessionOptions ->
    IO (Ptr OrtSessionOptions)

#{def
  HsOrtSessionOptions* _wrap_COrtSessionOptions(
    const OrtApi* ortApi,
    COrtSessionOptions* ortSessionOptions
  ) {
    HsOrtSessionOptions *out = malloc(sizeof *out);
    out->ortApi = ortApi;
    out->ortSessionOptions = ortSessionOptions;
    return out;
  }
}

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h &_wrap_OrtApi_ReleaseSessionOptions"
  _wrap_OrtApi_ReleaseSessionOptions ::
    FunPtr (
      Ptr OrtSessionOptions ->
      IO ()
    )

#{def
  void _wrap_OrtApi_ReleaseSessionOptions(HsOrtSessionOptions* ortSessionOptions) {
    ortSessionOptions->ortApi->ReleaseSessionOptions(ortSessionOptions->ortSessionOptions);
    free(ortSessionOptions);
  }
}

-------------------------------------------------------------------------------
-- OrtStatus

data
  {-# CTYPE "onnxruntime_c_api.h" "OrtStatus" #-}
  OrtStatus

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_OrtApi_ReleaseStatus"
  _wrap_OrtApi_ReleaseStatus ::
    OrtApi ->
    Ptr OrtStatus ->
    IO ()

#{def
  void _wrap_OrtApi_ReleaseStatus(const OrtApi* ortApi, OrtStatus* ortStatus) {
    ortApi->ReleaseStatus(ortStatus);
  }
}

data OrtError = OrtError
  { ortErrorCode    :: {-# UNPACK #-} !OrtErrorCode
  , ortErrorMessage :: {-# UNPACK #-} !ByteString
  }
  deriving stock (Eq, Show)

instance Exception OrtError where
  displayException ortError =
    printf "ERROR %d: %s"
      ortError.ortErrorCode.unOrtErrorCode
      (BSC.unpack ortError.ortErrorMessage)

handleOrtStatus ::
  OrtApi ->
  Ptr OrtStatus ->
  IO a ->
  IO a
handleOrtStatus ortApi ortStatusPtr action = do
  let actionOrError = do
        ortErrorCode <- ortApiGetErrorCode ortApi ortStatusPtr
        if ortErrorCode == OrtOk then action else do
          ortErrorMessage <- ortApiGetErrorMessage ortApi ortStatusPtr
          throwIO OrtError {..}
  let cleanupStatus =
        _wrap_OrtApi_ReleaseStatus ortApi ortStatusPtr
  actionOrError `finally` cleanupStatus

-------------------------------------------------------------------------------
-- OrtTensorTypeAndShapeInfo

data
  {-# CTYPE "onnxruntime_c_api.h" "COrtTensorTypeAndShapeInfo" #-}
  COrtTensorTypeAndShapeInfo

#{def
  typedef OrtTensorTypeAndShapeInfo COrtTensorTypeAndShapeInfo;
}

newtype
  {-# CTYPE "Onnxruntime/CApi_hsc.h" "HsOrtTensorTypeAndShapeInfo" #-}
  OrtTensorTypeAndShapeInfo = OrtTensorTypeAndShapeInfo { ortTensorTypeAndShapeInfoForeignPtr :: ForeignPtr OrtTensorTypeAndShapeInfo }

#{def
  typedef struct HsOrtTensorTypeAndShapeInfo {
    const OrtApi* ortApi;
    COrtTensorTypeAndShapeInfo* ortTensorTypeAndShapeInfo;
  } HsOrtTensorTypeAndShapeInfo;
}

instance HasOrtApi OrtTensorTypeAndShapeInfo where
  getOrtApi ortTensorTypeAndShapeInfo =
    withOrtTensorTypeAndShapeInfoPtr ortTensorTypeAndShapeInfo $ \ortTensorTypeAndShapeInfoPtr ->
      OrtApi <$> #{peek HsOrtTensorTypeAndShapeInfo, ortApi} ortTensorTypeAndShapeInfoPtr

-- | Internal helper.
withOrtTensorTypeAndShapeInfoPtr ::
  OrtTensorTypeAndShapeInfo ->
  (Ptr OrtTensorTypeAndShapeInfo -> IO a) ->
  IO a
withOrtTensorTypeAndShapeInfoPtr ortTensorTypeAndShapeInfo =
  withForeignPtr ortTensorTypeAndShapeInfo.ortTensorTypeAndShapeInfoForeignPtr

-- | Internal helper.
wrapCOrtTensorTypeAndShapeInfo ::
  OrtApi ->
  Ptr COrtTensorTypeAndShapeInfo ->
  IO OrtTensorTypeAndShapeInfo
wrapCOrtTensorTypeAndShapeInfo ortApi rawOrtTensorTypeAndShapeInfoPtr = do
  ortTensorTypeAndShapeInfoPtr <- _wrap_COrtTensorTypeAndShapeInfo ortApi.ortApiConstPtr rawOrtTensorTypeAndShapeInfoPtr
  ortTensorTypeAndShapeInfoForeignPtr <- newForeignPtr _wrap_OrtApi_ReleaseTensorTypeAndShapeInfo ortTensorTypeAndShapeInfoPtr
  pure $ OrtTensorTypeAndShapeInfo ortTensorTypeAndShapeInfoForeignPtr

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_COrtTensorTypeAndShapeInfo"
  _wrap_COrtTensorTypeAndShapeInfo ::
    ConstPtr OrtApi ->
    Ptr COrtTensorTypeAndShapeInfo ->
    IO (Ptr OrtTensorTypeAndShapeInfo)

#{def
  HsOrtTensorTypeAndShapeInfo* _wrap_COrtTensorTypeAndShapeInfo(
    const OrtApi* ortApi,
    COrtTensorTypeAndShapeInfo* ortTensorTypeAndShapeInfo
  ) {
    HsOrtTensorTypeAndShapeInfo *out = malloc(sizeof *out);
    out->ortApi = ortApi;
    out->ortTensorTypeAndShapeInfo = ortTensorTypeAndShapeInfo;
    return out;
  }
}

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h &_wrap_OrtApi_ReleaseTensorTypeAndShapeInfo"
  _wrap_OrtApi_ReleaseTensorTypeAndShapeInfo ::
    FunPtr (
      Ptr OrtTensorTypeAndShapeInfo ->
      IO ()
    )

#{def
  void _wrap_OrtApi_ReleaseTensorTypeAndShapeInfo(HsOrtTensorTypeAndShapeInfo* ortTensorTypeAndShapeInfo) {
    ortTensorTypeAndShapeInfo->ortApi->ReleaseTensorTypeAndShapeInfo(ortTensorTypeAndShapeInfo->ortTensorTypeAndShapeInfo);
    free(ortTensorTypeAndShapeInfo);
  }
}

-------------------------------------------------------------------------------
-- OrtTypeInfo

data
  {-# CTYPE "onnxruntime_c_api.h" "COrtTypeInfo" #-}
  COrtTypeInfo

#{def
  typedef OrtTypeInfo COrtTypeInfo;
}

newtype
  {-# CTYPE "Onnxruntime/CApi_hsc.h" "HsOrtTypeInfo" #-}
  OrtTypeInfo = OrtTypeInfo { ortTypeInfoForeignPtr :: ForeignPtr OrtTypeInfo }

#{def
  typedef struct HsOrtTypeInfo {
    const OrtApi* ortApi;
    COrtTypeInfo* ortTypeInfo;
  } HsOrtTypeInfo;
}

instance HasOrtApi OrtTypeInfo where
  getOrtApi ortTypeInfo =
    withOrtTypeInfoPtr ortTypeInfo $ \ortTypeInfoPtr ->
      OrtApi <$> #{peek HsOrtTypeInfo, ortApi} ortTypeInfoPtr

-- | Internal helper.
withOrtTypeInfoPtr ::
  OrtTypeInfo ->
  (Ptr OrtTypeInfo -> IO a) ->
  IO a
withOrtTypeInfoPtr ortTypeInfo =
  withForeignPtr ortTypeInfo.ortTypeInfoForeignPtr

-- | Internal helper.
wrapCOrtTypeInfo ::
  OrtApi ->
  Ptr COrtTypeInfo ->
  IO OrtTypeInfo
wrapCOrtTypeInfo ortApi rawOrtTypeInfoPtr = do
  ortTypeInfoPtr <- _wrap_COrtTypeInfo ortApi.ortApiConstPtr rawOrtTypeInfoPtr
  ortTypeInfoForeignPtr <- newForeignPtr _wrap_OrtApi_ReleaseTypeInfo ortTypeInfoPtr
  pure $ OrtTypeInfo ortTypeInfoForeignPtr

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_COrtTypeInfo"
  _wrap_COrtTypeInfo ::
    ConstPtr OrtApi ->
    Ptr COrtTypeInfo ->
    IO (Ptr OrtTypeInfo)

#{def
  HsOrtTypeInfo* _wrap_COrtTypeInfo(
    const OrtApi* ortApi,
    COrtTypeInfo* ortTypeInfo
  ) {
    HsOrtTypeInfo *out = malloc(sizeof *out);
    out->ortApi = ortApi;
    out->ortTypeInfo = ortTypeInfo;
    return out;
  }
}

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h &_wrap_OrtApi_ReleaseTypeInfo"
  _wrap_OrtApi_ReleaseTypeInfo ::
    FunPtr (
      Ptr OrtTypeInfo ->
      IO ()
    )

#{def
  void _wrap_OrtApi_ReleaseTypeInfo(HsOrtTypeInfo* ortTypeInfo) {
    ortTypeInfo->ortApi->ReleaseTypeInfo(ortTypeInfo->ortTypeInfo);
    free(ortTypeInfo);
  }
}

-------------------------------------------------------------------------------
-- OrtRunOptions

data
  {-# CTYPE "onnxruntime_c_api.h" "COrtRunOptions" #-}
  COrtRunOptions

#{def
  typedef OrtRunOptions COrtRunOptions;
}

newtype
  {-# CTYPE "Onnxruntime/CApi_hsc.h" "HsOrtRunOptions" #-}
  OrtRunOptions = OrtRunOptions { ortRunOptionsForeignPtr :: ForeignPtr OrtRunOptions }

#{def
  typedef struct HsOrtRunOptions {
    const OrtApi* ortApi;
    COrtRunOptions* ortRunOptions;
  } HsOrtRunOptions;
}

instance HasOrtApi OrtRunOptions where
  getOrtApi ortRunOptions =
    withOrtRunOptionsPtr ortRunOptions $ \ortRunOptionsPtr ->
      OrtApi <$> #{peek HsOrtRunOptions, ortApi} ortRunOptionsPtr

-- | Internal helper.
withOrtRunOptionsPtr ::
  OrtRunOptions ->
  (Ptr OrtRunOptions -> IO a) ->
  IO a
withOrtRunOptionsPtr ortRunOptions =
  withForeignPtr ortRunOptions.ortRunOptionsForeignPtr

-- | Internal helper.
withCOrtRunOptionsPtr ::
  OrtRunOptions ->
  (Ptr COrtRunOptions -> IO a) ->
  IO a
withCOrtRunOptionsPtr ortRunOptions action =
  withOrtRunOptionsPtr ortRunOptions $ \ortRunOptionsPtr -> do
    cOrtRunOptionsPtr <- #{peek HsOrtRunOptions, ortRunOptions} ortRunOptionsPtr
    action cOrtRunOptionsPtr

-- | Internal helper.
wrapCOrtRunOptions ::
  OrtApi ->
  Ptr COrtRunOptions ->
  IO OrtRunOptions
wrapCOrtRunOptions ortApi rawOrtRunOptionsPtr = do
  ortRunOptionsPtr <- _wrap_COrtRunOptions ortApi.ortApiConstPtr rawOrtRunOptionsPtr
  ortRunOptionsForeignPtr <- newForeignPtr _wrap_OrtApi_ReleaseRunOptions ortRunOptionsPtr
  pure $ OrtRunOptions ortRunOptionsForeignPtr

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_COrtRunOptions"
  _wrap_COrtRunOptions ::
    ConstPtr OrtApi ->
    Ptr COrtRunOptions ->
    IO (Ptr OrtRunOptions)

#{def
  HsOrtRunOptions* _wrap_COrtRunOptions(
    const OrtApi* ortApi,
    COrtRunOptions* ortRunOptions
  ) {
    HsOrtRunOptions *out = malloc(sizeof *out);
    out->ortApi = ortApi;
    out->ortRunOptions = ortRunOptions;
    return out;
  }
}

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h &_wrap_OrtApi_ReleaseRunOptions"
  _wrap_OrtApi_ReleaseRunOptions ::
    FunPtr (
      Ptr OrtRunOptions ->
      IO ()
    )

#{def
  void _wrap_OrtApi_ReleaseRunOptions(HsOrtRunOptions* ortRunOptions) {
    ortRunOptions->ortApi->ReleaseRunOptions(ortRunOptions->ortRunOptions);
    free(ortRunOptions);
  }
}

-------------------------------------------------------------------------------
-- OrtValue

data
  {-# CTYPE "onnxruntime_c_api.h" "COrtValue" #-}
  COrtValue

#{def
  typedef OrtValue COrtValue;
}

newtype
  {-# CTYPE "Onnxruntime/CApi_hsc.h" "HsOrtValue" #-}
  OrtValue = OrtValue { ortValueForeignPtr :: ForeignPtr OrtValue }

#{def
  typedef struct HsOrtValue {
    const OrtApi* ortApi;
    COrtValue* ortValue;
  } HsOrtValue;
}

instance HasOrtApi OrtValue where
  getOrtApi ortValue =
    withOrtValuePtr ortValue $ \ortValuePtr ->
      OrtApi <$> #{peek HsOrtValue, ortApi} ortValuePtr

-- | Internal helper.
withOrtValuePtr ::
  OrtValue ->
  (Ptr OrtValue -> IO a) ->
  IO a
withOrtValuePtr ortValue =
  withForeignPtr ortValue.ortValueForeignPtr

-- | Internal helper.
withCOrtValuePtr ::
  OrtValue ->
  (Ptr COrtValue -> IO a) ->
  IO a
withCOrtValuePtr ortValue action =
  withOrtValuePtr ortValue $ \ortValuePtr -> do
    cOrtValuePtr <- #{peek HsOrtValue, ortValue} ortValuePtr
    action cOrtValuePtr

-- | Internal helper.
wrapCOrtValue ::
  OrtApi ->
  Ptr COrtValue ->
  IO OrtValue
wrapCOrtValue ortApi rawOrtValuePtr = do
  ortValuePtr <- _wrap_COrtValue ortApi.ortApiConstPtr rawOrtValuePtr
  ortValueForeignPtr <- newForeignPtr _wrap_OrtApi_ReleaseValue ortValuePtr
  pure $ OrtValue ortValueForeignPtr

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_COrtValue"
  _wrap_COrtValue ::
    ConstPtr OrtApi ->
    Ptr COrtValue ->
    IO (Ptr OrtValue)

#{def
  HsOrtValue* _wrap_COrtValue(
    const OrtApi* ortApi,
    COrtValue* ortValue
  ) {
    HsOrtValue *out = malloc(sizeof *out);
    out->ortApi = ortApi;
    out->ortValue = ortValue;
    return out;
  }
}

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h &_wrap_OrtApi_ReleaseValue"
  _wrap_OrtApi_ReleaseValue ::
    FunPtr (
      Ptr OrtValue ->
      IO ()
    )

#{def
  void _wrap_OrtApi_ReleaseValue(HsOrtValue* ortValue) {
    ortValue->ortApi->ReleaseValue(ortValue->ortValue);
    free(ortValue);
  }
}

-------------------------------------------------------------------------------
-- ONNX Runtime: API Function
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- OrtApi::GetErrorCode

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_OrtApi_GetErrorCode"
  ortApiGetErrorCode ::
    OrtApi ->
    Ptr OrtStatus ->
    IO OrtErrorCode

#{def
  OrtErrorCode _wrap_OrtApi_GetErrorCode(const OrtApi* ortApi, OrtStatus* ortStatus) {
    return ortApi->GetErrorCode(ortStatus);
  }
}

-------------------------------------------------------------------------------
-- OrtApi::GetErrorMessage

ortApiGetErrorMessageAsString ::
  OrtApi ->
  Ptr OrtStatus ->
  IO String
ortApiGetErrorMessageAsString ortApi ortStatusPtr = do
  ConstPtr msgPtr <- _wrap_OrtApi_GetErrorMessage ortApi ortStatusPtr
  peekCString msgPtr

ortApiGetErrorMessage ::
  OrtApi ->
  Ptr OrtStatus ->
  IO ByteString
ortApiGetErrorMessage ortApi ortStatusPtr = do
  ConstPtr msgPtr <- _wrap_OrtApi_GetErrorMessage ortApi ortStatusPtr
  BSU.unsafePackCString msgPtr

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_OrtApi_GetErrorMessage"
  _wrap_OrtApi_GetErrorMessage ::
    OrtApi ->
    Ptr OrtStatus ->
    IO (ConstPtr CChar)

#{def
  const char* _wrap_OrtApi_GetErrorMessage(const OrtApi* ortApi, OrtStatus* ortStatus) {
    return ortApi->GetErrorMessage(ortStatus);
  }
}

-------------------------------------------------------------------------------
-- OrtApi::CreateEnv

{- |
> ORT_API2_STATUS(CreateEnv,
>   OrtLoggingLevel log_severity_level,
>   _In_ const char* logid,
>   _Outptr_ OrtEnv** out
> );
-}
ortApiCreateEnv ::
  OrtApi ->
  OrtLoggingLevel ->
  ConstPtr CChar ->
  IO OrtEnv
ortApiCreateEnv ortApi logSeverityLevel logidConstPtr = do
  alloca $ \outPtr -> do
    ortStatusPtr <-
      _wrap_OrtApi_CreateEnv
        ortApi.ortApiConstPtr
        logSeverityLevel
        logidConstPtr
        outPtr
    handleOrtStatus ortApi ortStatusPtr $ do
      wrapCOrtEnv ortApi
        =<< peek outPtr

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_OrtApi_CreateEnv"
  _wrap_OrtApi_CreateEnv ::
    ConstPtr OrtApi ->
    OrtLoggingLevel ->
    ConstPtr CChar ->
    Ptr (Ptr COrtEnv) ->
    IO (Ptr OrtStatus)

#{def
  OrtStatus* _wrap_OrtApi_CreateEnv(
    const OrtApi* ortApi,
    OrtLoggingLevel logSeverityLevel,
    const char* logid,
    OrtEnv** out
  ) {
    return ortApi->CreateEnv(logSeverityLevel, logid, out);
  }
}

-------------------------------------------------------------------------------
-- OrtApi::CreateEnvWithCustomLogger

{-
> ORT_API2_STATUS(CreateEnvWithCustomLogger,
>   _In_ OrtLoggingFunction logging_function,
>   _In_opt_ void* logger_param,
>   _In_ OrtLoggingLevel log_severity_level,
>   _In_ const char* logid,
>   _Outptr_ OrtEnv** out
> );
-}

-------------------------------------------------------------------------------
-- OrtApi::EnableTelemetryEvents

{-
> ORT_API2_STATUS(EnableTelemetryEvents,
>   _In_ const OrtEnv* env
>);
-}

-------------------------------------------------------------------------------
-- OrtApi::DisableTelemetryEvents

{-
> ORT_API2_STATUS(DisableTelemetryEvents,
>   _In_ const OrtEnv* env
>);
-}

-------------------------------------------------------------------------------
-- OrtApi::CreateSession

{-
> ORT_API2_STATUS(CreateSession,
>   _In_ const OrtEnv* env,
>   _In_ const ORTCHAR_T* model_path,
>   _In_ const OrtSessionOptions* options,
>   _Outptr_ OrtSession** out
> );
-}
ortApiCreateSession ::
  OrtEnv ->
  FilePath ->
  OrtSessionOptions ->
  IO OrtSession
ortApiCreateSession ortEnv modelPath options = do
  ortApi <- getOrtApi ortEnv
  alloca $ \outPtr -> do
    withCOrtEnvPtr ortEnv $ \cOrtEnvPtr -> do
      withCString modelPath $ \modelPathPtr -> do
        withCOrtSessionOptionsPtr options $ \cOrtSessionOptionsPtr -> do
          ortStatusPtr <-
            _wrap_OrtApi_CreateSession
              ortApi
              cOrtEnvPtr
              (ConstPtr modelPathPtr) -- NOTE: This is unsafe.
              cOrtSessionOptionsPtr
              outPtr
          handleOrtStatus ortApi ortStatusPtr $ do
            wrapCOrtSession ortApi
              =<< peek outPtr

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_OrtApi_CreateSession"
  _wrap_OrtApi_CreateSession ::
    OrtApi ->
    Ptr COrtEnv ->
    ConstPtr CChar ->
    Ptr COrtSessionOptions ->
    Ptr (Ptr COrtSession) ->
    IO (Ptr OrtStatus)

#{def
  OrtStatus* _wrap_OrtApi_CreateSession(
    OrtApi* ortApi,
    COrtEnv* ortEnv,
    const ORTCHAR_T* modelPath,
    COrtSessionOptions* options,
    COrtSession** out
  ) {
    return ortApi->CreateSession(
      ortEnv,
      modelPath,
      options,
      out
    );
  }
}

-------------------------------------------------------------------------------
-- OrtApi::CreateSessionFromArray

{-
> ORT_API2_STATUS(CreateSessionFromArray,
>   _In_ const OrtEnv* env,
>   _In_ const void* model_data,
>   size_t model_data_length,
>   _In_ const OrtSessionOptions* options,
>   _Outptr_ OrtSession** out
> );
-}

-------------------------------------------------------------------------------
-- OrtApi::Run

{-
> ORT_API2_STATUS(Run,
>   _Inout_ OrtSession* session,
>   _In_opt_ const OrtRunOptions* run_options,
>   _In_reads_(input_len) const char* const* input_names,
>   _In_reads_(input_len) const OrtValue* const* inputs,
>   size_t input_len,
>   _In_reads_(output_names_len) const char* const* output_names,
>   size_t output_names_len,
>   _Inout_updates_all_(output_names_len) OrtValue** outputs
> );
-}

-------------------------------------------------------------------------------
-- OrtApi::CreateSessionOptions

{- |
> ORT_API2_STATUS(CreateSessionOptions,
>   _Outptr_ OrtSessionOptions** options
> );
-}
ortApiCreateSessionOptions ::
  OrtApi ->
  IO OrtSessionOptions
ortApiCreateSessionOptions ortApi = do
  alloca $ \outPtr -> do
    ortStatusPtr <-
      _wrap_OrtApi_CreateSessionOptions
        ortApi.ortApiConstPtr
        outPtr
    handleOrtStatus ortApi ortStatusPtr $ do
      cOrtSessionOptionsPtr <- peek outPtr
      wrapCOrtSessionOptions ortApi cOrtSessionOptionsPtr

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_OrtApi_CreateSessionOptions"
  _wrap_OrtApi_CreateSessionOptions ::
    ConstPtr OrtApi ->
    Ptr (Ptr COrtSessionOptions) ->
    IO (Ptr OrtStatus)

#{def
  OrtStatus* _wrap_OrtApi_CreateSessionOptions(
    const OrtApi* ortApi,
    COrtSessionOptions** out
  ) {
    return ortApi->CreateSessionOptions(out);
  }
}

-------------------------------------------------------------------------------
-- OrtApi::CloneSessionOptions

{- |
> ORT_API2_STATUS(CloneSessionOptions,
>   _In_ const OrtSessionOptions* in_options,
>   _Outptr_ OrtSessionOptions** out_options
  );
-}
ortApiCloneSessionOptions ::
  OrtSessionOptions ->
  IO OrtSessionOptions
ortApiCloneSessionOptions inOptions = do
  ortApi <- getOrtApi inOptions
  withOrtSessionOptionsPtr inOptions $ \inOptionsPtr -> do
    alloca $ \outPtr -> do
      ortStatusPtr <-
        _wrap_OrtApi_CloneSessionOptions
          inOptionsPtr
          outPtr
      handleOrtStatus ortApi ortStatusPtr $
        wrapCOrtSessionOptions ortApi
          =<< peek outPtr

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_OrtApi_CloneSessionOptions"
  _wrap_OrtApi_CloneSessionOptions ::
    Ptr OrtSessionOptions ->
    Ptr (Ptr COrtSessionOptions) ->
    IO (Ptr OrtStatus)

#{def
  OrtStatus* _wrap_OrtApi_CloneSessionOptions(
    HsOrtSessionOptions* inOptions,
    COrtSessionOptions** out
  ) {
    return inOptions->ortApi->CloneSessionOptions(
      inOptions->ortSessionOptions,
      out
    );
  }
}

-------------------------------------------------------------------------------
-- OrtApi::SetOptimizedModelFilePath

{- |
> ORT_API2_STATUS(SetOptimizedModelFilePath,
>   _Inout_ OrtSessionOptions* options,
>   _In_ const ORTCHAR_T* optimized_model_filepath
> );
-}
ortApiSetOptimizedModelFilePath ::
  OrtSessionOptions ->
  FilePath ->
  IO ()
ortApiSetOptimizedModelFilePath options optimizedModelFilepath = do
  ortApi <- getOrtApi options
  withCString optimizedModelFilepath $ \optimizedModelFilepathPtr ->
    withOrtSessionOptionsPtr options $ \optionsPtr -> do
      ortStatusPtr <-
        _wrap_OrtApi_SetOptimizedModelFilePath
          optionsPtr
          (ConstPtr optimizedModelFilepathPtr) -- NOTE: This is unsafe.
      handleOrtStatus ortApi ortStatusPtr $ do
        pure ()

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_OrtApi_SetOptimizedModelFilePath"
  _wrap_OrtApi_SetOptimizedModelFilePath ::
    Ptr OrtSessionOptions ->
    ConstPtr CChar ->
    IO (Ptr OrtStatus)

#{def
  OrtStatus* _wrap_OrtApi_SetOptimizedModelFilePath(
    HsOrtSessionOptions* options,
    const ORTCHAR_T* optimizedModelFilepath
  ) {
    return options->ortApi->SetOptimizedModelFilePath(
      options->ortSessionOptions,
      optimizedModelFilepath
    );
  }
}

-------------------------------------------------------------------------------
-- OrtApi::SetSessionExecutionMode

{-
> ORT_API2_STATUS(SetSessionExecutionMode,
>   _Inout_ OrtSessionOptions* options,
>   ExecutionMode execution_mode
> );
-}
ortApiSetSessionExecutionMode ::
  OrtSessionOptions ->
  ExecutionMode ->
  IO ()
ortApiSetSessionExecutionMode options executionMode = do
  ortApi <- getOrtApi options
  withOrtSessionOptionsPtr options $ \optionsPtr -> do
    ortStatusPtr <-
      _wrap_OrtApi_SetSessionExecutionMode
        optionsPtr
        executionMode
    handleOrtStatus ortApi ortStatusPtr $ do
      pure ()

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_OrtApi_SetSessionExecutionMode"
  _wrap_OrtApi_SetSessionExecutionMode ::
    Ptr OrtSessionOptions ->
    ExecutionMode ->
    IO (Ptr OrtStatus)

#{def
  OrtStatus* _wrap_OrtApi_SetSessionExecutionMode(
    HsOrtSessionOptions* options,
    int executionMode
  ) {
    return options->ortApi->SetSessionExecutionMode(
      options->ortSessionOptions,
      executionMode
    );
  }
}

-------------------------------------------------------------------------------
-- OrtApi::EnableProfiling

{- |
> ORT_API2_STATUS(EnableProfiling,
>   _Inout_ OrtSessionOptions* options,
>   _In_ const ORTCHAR_T* profile_file_prefix
> );
-}
ortApiEnableProfiling ::
  OrtSessionOptions ->
  FilePath ->
  IO ()
ortApiEnableProfiling options profileFilePrefix = do
  ortApi <- getOrtApi options
  withCString profileFilePrefix $ \profileFilePrefixPtr ->
    withOrtSessionOptionsPtr options $ \optionsPtr -> do
      ortStatusPtr <-
        _wrap_OrtApi_EnableProfiling
          optionsPtr
          (ConstPtr profileFilePrefixPtr) -- NOTE: This is unsafe.
      handleOrtStatus ortApi ortStatusPtr $ do
        pure ()

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_OrtApi_EnableProfiling"
  _wrap_OrtApi_EnableProfiling ::
    Ptr OrtSessionOptions ->
    ConstPtr CChar ->
    IO (Ptr OrtStatus)

#{def
  OrtStatus* _wrap_OrtApi_EnableProfiling(
    HsOrtSessionOptions* options,
    const ORTCHAR_T* profileFilePrefix
  ) {
    return options->ortApi->EnableProfiling(
      options->ortSessionOptions,
      profileFilePrefix
    );
  }
}

-------------------------------------------------------------------------------
-- OrtApi::DisableProfiling

{- |
> ORT_API2_STATUS(DisableProfiling,
>   _Inout_ OrtSessionOptions* options
> );
-}
ortApiDisableProfiling ::
  OrtSessionOptions ->
  IO ()
ortApiDisableProfiling options = do
  ortApi <- getOrtApi options
  withOrtSessionOptionsPtr options $ \optionsPtr -> do
    ortStatusPtr <-
      _wrap_OrtApi_DisableProfiling
        optionsPtr
    handleOrtStatus ortApi ortStatusPtr $ do
      pure ()

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_OrtApi_DisableProfiling"
  _wrap_OrtApi_DisableProfiling ::
    Ptr OrtSessionOptions ->
    IO (Ptr OrtStatus)

#{def
  OrtStatus* _wrap_OrtApi_DisableProfiling(
    HsOrtSessionOptions* options
  ) {
    return options->ortApi->DisableProfiling(options->ortSessionOptions);
  }
}

-------------------------------------------------------------------------------
-- OrtApi::EnableMemPattern

{- |
> ORT_API2_STATUS(EnableMemPattern,
>   _Inout_ OrtSessionOptions* options
> );
-}
ortApiEnableMemPattern ::
  OrtSessionOptions ->
  IO ()
ortApiEnableMemPattern options = do
  ortApi <- getOrtApi options
  withOrtSessionOptionsPtr options $ \optionsPtr -> do
    ortStatusPtr <-
      _wrap_OrtApi_EnableMemPattern
        optionsPtr
    handleOrtStatus ortApi ortStatusPtr $ do
      pure ()

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_OrtApi_EnableMemPattern"
  _wrap_OrtApi_EnableMemPattern ::
    Ptr OrtSessionOptions ->
    IO (Ptr OrtStatus)

#{def
  OrtStatus* _wrap_OrtApi_EnableMemPattern(
    HsOrtSessionOptions* options
  ) {
    return options->ortApi->EnableMemPattern(options->ortSessionOptions);
  }
}

-------------------------------------------------------------------------------
-- OrtApi::DisableMemPattern

{- |
> ORT_API2_STATUS(DisableMemPattern,
>   _Inout_ OrtSessionOptions* options
> );
-}
ortApiDisableMemPattern ::
  OrtSessionOptions ->
  IO ()
ortApiDisableMemPattern options = do
  ortApi <- getOrtApi options
  withOrtSessionOptionsPtr options $ \optionsPtr -> do
    ortStatusPtr <-
      _wrap_OrtApi_DisableMemPattern
        optionsPtr
    handleOrtStatus ortApi ortStatusPtr $ do
      pure ()

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_OrtApi_DisableMemPattern"
  _wrap_OrtApi_DisableMemPattern ::
    Ptr OrtSessionOptions ->
    IO (Ptr OrtStatus)

#{def
  OrtStatus* _wrap_OrtApi_DisableMemPattern(
    HsOrtSessionOptions* options
  ) {
    return options->ortApi->DisableMemPattern(options->ortSessionOptions);
  }
}

-------------------------------------------------------------------------------
-- OrtApi::EnableCpuMemArena

{- |
> ORT_API2_STATUS(EnableCpuMemArena,
>   _Inout_ OrtSessionOptions* options
> );
-}
ortApiEnableCpuMemArena ::
  OrtSessionOptions ->
  IO ()
ortApiEnableCpuMemArena options = do
  ortApi <- getOrtApi options
  withOrtSessionOptionsPtr options $ \optionsPtr -> do
    ortStatusPtr <-
      _wrap_OrtApi_EnableCpuMemArena
        optionsPtr
    handleOrtStatus ortApi ortStatusPtr $ do
      pure ()

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_OrtApi_EnableCpuMemArena"
  _wrap_OrtApi_EnableCpuMemArena ::
    Ptr OrtSessionOptions ->
    IO (Ptr OrtStatus)

#{def
  OrtStatus* _wrap_OrtApi_EnableCpuMemArena(
    HsOrtSessionOptions* options
  ) {
    return options->ortApi->EnableCpuMemArena(options->ortSessionOptions);
  }
}

-------------------------------------------------------------------------------
-- OrtApi::DisableCpuMemArena

{- |
> ORT_API2_STATUS(DisableCpuMemArena,
>   _Inout_ OrtSessionOptions* options
> );
-}
ortApiDisableCpuMemArena ::
  OrtSessionOptions ->
  IO ()
ortApiDisableCpuMemArena options = do
  ortApi <- getOrtApi options
  withOrtSessionOptionsPtr options $ \optionsPtr -> do
    ortStatusPtr <-
      _wrap_OrtApi_DisableCpuMemArena
        optionsPtr
    handleOrtStatus ortApi ortStatusPtr $ do
      pure ()

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_OrtApi_DisableCpuMemArena"
  _wrap_OrtApi_DisableCpuMemArena ::
    Ptr OrtSessionOptions ->
    IO (Ptr OrtStatus)

#{def
  OrtStatus* _wrap_OrtApi_DisableCpuMemArena(
    HsOrtSessionOptions* options
  ) {
    return options->ortApi->DisableCpuMemArena(options->ortSessionOptions);
  }
}

-------------------------------------------------------------------------------
-- OrtApi::SetSessionLogId

{- |
> ORT_API2_STATUS(SetSessionLogId,
>   _Inout_ OrtSessionOptions* options,
>   const char* logid
> );
-}
ortApiSetSessionLogId ::
  OrtSessionOptions ->
  String ->
  IO ()
ortApiSetSessionLogId options logid = do
  ortApi <- getOrtApi options
  withCString logid $ \logidPtr ->
    withOrtSessionOptionsPtr options $ \optionsPtr -> do
      ortStatusPtr <-
        _wrap_OrtApi_SetSessionLogId
          optionsPtr
          (ConstPtr logidPtr) -- NOTE: This is unsafe.
      handleOrtStatus ortApi ortStatusPtr $ do
        pure ()

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_OrtApi_SetSessionLogId"
  _wrap_OrtApi_SetSessionLogId ::
    Ptr OrtSessionOptions ->
    ConstPtr CChar ->
    IO (Ptr OrtStatus)

#{def
  OrtStatus* _wrap_OrtApi_SetSessionLogId(
    HsOrtSessionOptions* options,
    const char* logid
  ) {
    return options->ortApi->SetSessionLogId(
      options->ortSessionOptions,
      logid
    );
  }
}

-------------------------------------------------------------------------------
-- OrtApi::SetSessionLogVerbosityLevel

{- |
> ORT_API2_STATUS(SetSessionLogVerbosityLevel,
>   _Inout_ OrtSessionOptions* options,
>   int session_log_verbosity_level
> );
-}
ortApiSetSessionLogVerbosityLevel ::
  OrtSessionOptions ->
  Int ->
  IO ()
ortApiSetSessionLogVerbosityLevel options sessionLogVerbosityLevel = do
  ortApi <- getOrtApi options
  withOrtSessionOptionsPtr options $ \optionsPtr -> do
    ortStatusPtr <-
      _wrap_OrtApi_SetSessionLogVerbosityLevel
        optionsPtr
        (fromIntegral sessionLogVerbosityLevel)
    handleOrtStatus ortApi ortStatusPtr $ do
      pure ()

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_OrtApi_SetSessionLogVerbosityLevel"
  _wrap_OrtApi_SetSessionLogVerbosityLevel ::
    Ptr OrtSessionOptions ->
    CInt ->
    IO (Ptr OrtStatus)

#{def
  OrtStatus* _wrap_OrtApi_SetSessionLogVerbosityLevel(
    HsOrtSessionOptions* options,
    int sessionLogVerbosityLevel
  ) {
    return options->ortApi->SetSessionLogVerbosityLevel(
      options->ortSessionOptions,
      sessionLogVerbosityLevel
    );
  }
}

-------------------------------------------------------------------------------
-- OrtApi::SetSessionLogSeverityLevel

{-
> ORT_API2_STATUS(SetSessionLogSeverityLevel,
>   _Inout_ OrtSessionOptions* options,
>   int session_log_severity_level
> );
-}
ortApiSetSessionLogSeverityLevel ::
  OrtSessionOptions ->
  Int ->
  IO ()
ortApiSetSessionLogSeverityLevel options sessionLogSeverityLevel = do
  ortApi <- getOrtApi options
  withOrtSessionOptionsPtr options $ \optionsPtr -> do
    ortStatusPtr <-
      _wrap_OrtApi_SetSessionLogSeverityLevel
        optionsPtr
        (fromIntegral sessionLogSeverityLevel)
    handleOrtStatus ortApi ortStatusPtr $ do
      pure ()

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_OrtApi_SetSessionLogSeverityLevel"
  _wrap_OrtApi_SetSessionLogSeverityLevel ::
    Ptr OrtSessionOptions ->
    CInt ->
    IO (Ptr OrtStatus)

#{def
  OrtStatus* _wrap_OrtApi_SetSessionLogSeverityLevel(
    HsOrtSessionOptions* options,
    int sessionLogSeverityLevel
  ) {
    return options->ortApi->SetSessionLogSeverityLevel(
      options->ortSessionOptions,
      sessionLogSeverityLevel
    );
  }
}

-------------------------------------------------------------------------------
-- OrtApi::SetSessionGraphOptimizationLevel

{-
> ORT_API2_STATUS(SetSessionGraphOptimizationLevel,
>   _Inout_ OrtSessionOptions* options,
>   GraphOptimizationLevel graph_optimization_level
> );
-}
ortApiSetSessionGraphOptimizationLevel ::
  OrtSessionOptions ->
  GraphOptimizationLevel ->
  IO ()
ortApiSetSessionGraphOptimizationLevel options graphOptimizationlevel = do
  ortApi <- getOrtApi options
  withOrtSessionOptionsPtr options $ \optionsPtr -> do
    ortStatusPtr <-
      _wrap_OrtApi_SetSessionGraphOptimizationLevel
        optionsPtr
        graphOptimizationlevel
    handleOrtStatus ortApi ortStatusPtr $ do
      pure ()

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_OrtApi_SetSessionGraphOptimizationLevel"
  _wrap_OrtApi_SetSessionGraphOptimizationLevel ::
    Ptr OrtSessionOptions ->
    GraphOptimizationLevel ->
    IO (Ptr OrtStatus)

#{def
  OrtStatus* _wrap_OrtApi_SetSessionGraphOptimizationLevel(
    HsOrtSessionOptions* options,
    int graphOptimizationlevel
  ) {
    return options->ortApi->SetSessionGraphOptimizationLevel(
      options->ortSessionOptions,
      graphOptimizationlevel
    );
  }
}

-------------------------------------------------------------------------------
-- OrtApi::SetIntraOpNumThreads

{- |
> ORT_API2_STATUS(SetIntraOpNumThreads,
>   _Inout_ OrtSessionOptions* options,
>   int intra_op_num_threads
> );
-}
ortApiSetIntraOpNumThreads ::
  OrtSessionOptions ->
  Int ->
  IO ()
ortApiSetIntraOpNumThreads options intraOpNumThreads = do
  ortApi <- getOrtApi options
  withOrtSessionOptionsPtr options $ \optionsPtr -> do
    ortStatusPtr <-
      _wrap_OrtApi_SetIntraOpNumThreads
        optionsPtr
        (fromIntegral intraOpNumThreads)
    handleOrtStatus ortApi ortStatusPtr $ do
      pure ()

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_OrtApi_SetIntraOpNumThreads"
  _wrap_OrtApi_SetIntraOpNumThreads ::
    Ptr OrtSessionOptions ->
    CInt ->
    IO (Ptr OrtStatus)

#{def
  OrtStatus* _wrap_OrtApi_SetIntraOpNumThreads(
    HsOrtSessionOptions* options,
    int intraOpNumThreads
  ) {
    return options->ortApi->SetIntraOpNumThreads(
      options->ortSessionOptions,
      intraOpNumThreads
    );
  }
}

-------------------------------------------------------------------------------
-- OrtApi::SetInterOpNumThreads

{-
> ORT_API2_STATUS(SetInterOpNumThreads,
>   _Inout_ OrtSessionOptions* options,
>   int inter_op_num_threads
> );
-}
ortApiSetInterOpNumThreads ::
  OrtSessionOptions ->
  Int ->
  IO ()
ortApiSetInterOpNumThreads options interOpNumThreads = do
  ortApi <- getOrtApi options
  withOrtSessionOptionsPtr options $ \optionsPtr -> do
    ortStatusPtr <-
      _wrap_OrtApi_SetInterOpNumThreads
        optionsPtr
        (fromIntegral interOpNumThreads)
    handleOrtStatus ortApi ortStatusPtr $ do
      pure ()

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_OrtApi_SetInterOpNumThreads"
  _wrap_OrtApi_SetInterOpNumThreads ::
    Ptr OrtSessionOptions ->
    CInt ->
    IO (Ptr OrtStatus)

#{def
  OrtStatus* _wrap_OrtApi_SetInterOpNumThreads(
    HsOrtSessionOptions* options,
    int interOpNumThreads
  ) {
    return options->ortApi->SetInterOpNumThreads(
      options->ortSessionOptions,
      interOpNumThreads
    );
  }
}

-------------------------------------------------------------------------------
-- OrtApi::CreateCustomOpDomain

-- ORT_API2_STATUS(CreateCustomOpDomain, _In_ const char* domain, _Outptr_ OrtCustomOpDomain** out);

-------------------------------------------------------------------------------
-- OrtApi::CustomOpDomain_Add

-- ORT_API2_STATUS(CustomOpDomain_Add, _Inout_ OrtCustomOpDomain* custom_op_domain, _In_ const OrtCustomOp* op);

-------------------------------------------------------------------------------
-- OrtApi::AddCustomOpDomain

-- ORT_API2_STATUS(AddCustomOpDomain, _Inout_ OrtSessionOptions* options, _In_ OrtCustomOpDomain* custom_op_domain);

-------------------------------------------------------------------------------
-- OrtApi::RegisterCustomOpsLibrary

-- ORT_API2_STATUS(RegisterCustomOpsLibrary, _Inout_ OrtSessionOptions* options, _In_ const char* library_path, _Outptr_ void** library_handle);

-------------------------------------------------------------------------------
-- OrtApi::SessionGetInputCount

-- ORT_API2_STATUS(SessionGetInputCount, _In_ const OrtSession* session, _Out_ size_t* out);

-------------------------------------------------------------------------------
-- OrtApi::SessionGetOutputCount

-- ORT_API2_STATUS(SessionGetOutputCount, _In_ const OrtSession* session, _Out_ size_t* out);

-------------------------------------------------------------------------------
-- OrtApi::SessionGetOverridableInitializerCount

-- ORT_API2_STATUS(SessionGetOverridableInitializerCount, _In_ const OrtSession* session, _Out_ size_t* out);

-------------------------------------------------------------------------------
-- OrtApi::SessionGetInputTypeInfo

-- ORT_API2_STATUS(SessionGetInputTypeInfo, _In_ const OrtSession* session, size_t index, _Outptr_ OrtTypeInfo** type_info);

-------------------------------------------------------------------------------
-- OrtApi::SessionGetOutputTypeInfo

-- ORT_API2_STATUS(SessionGetOutputTypeInfo, _In_ const OrtSession* session, size_t index, _Outptr_ OrtTypeInfo** type_info);

-------------------------------------------------------------------------------
-- OrtApi::SessionGetOverridableInitializerTypeInfo

-- ORT_API2_STATUS(SessionGetOverridableInitializerTypeInfo, _In_ const OrtSession* session, size_t index, _Outptr_ OrtTypeInfo** type_info);

-------------------------------------------------------------------------------
-- OrtApi::SessionGetInputName

-- ORT_API2_STATUS(SessionGetInputName, _In_ const OrtSession* session, size_t index, _Inout_ OrtAllocator* allocator, _Outptr_ char** value);

-------------------------------------------------------------------------------
-- OrtApi::SessionGetOutputName

-- ORT_API2_STATUS(SessionGetOutputName, _In_ const OrtSession* session, size_t index, _Inout_ OrtAllocator* allocator, _Outptr_ char** value);

-------------------------------------------------------------------------------
-- OrtApi::SessionGetOverridableInitializerName

-- ORT_API2_STATUS(SessionGetOverridableInitializerName, _In_ const OrtSession* session, size_t index, _Inout_ OrtAllocator* allocator, _Outptr_ char** value);

-------------------------------------------------------------------------------
-- OrtApi::CreateRunOptions

{- |
> ORT_API2_STATUS(CreateRunOptions,
>   _Outptr_ OrtRunOptions** options
> );
-}
ortApiCreateRunOptions ::
  OrtApi ->
  IO OrtRunOptions
ortApiCreateRunOptions ortApi = do
  alloca $ \outPtr -> do
    ortStatusPtr <-
      _wrap_OrtApi_CreateRunOptions
        ortApi.ortApiConstPtr
        outPtr
    handleOrtStatus ortApi ortStatusPtr $ do
      cOrtRunOptionsPtr <- peek outPtr
      wrapCOrtRunOptions ortApi cOrtRunOptionsPtr

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_OrtApi_CreateRunOptions"
  _wrap_OrtApi_CreateRunOptions ::
    ConstPtr OrtApi ->
    Ptr (Ptr COrtRunOptions) ->
    IO (Ptr OrtStatus)

#{def
  OrtStatus* _wrap_OrtApi_CreateRunOptions(
    const OrtApi* ortApi,
    COrtRunOptions** out
  ) {
    return ortApi->CreateRunOptions(out);
  }
}

-------------------------------------------------------------------------------
-- OrtApi::RunOptionsSetRunLogVerbosityLevel

{- |
> ORT_API2_STATUS(RunOptionsSetRunLogVerbosityLevel,
>   _Inout_ OrtRunOptions* options,
>   int log_verbosity_level
> );
-}
ortApiRunOptionsSetRunLogVerbosityLevel ::
  OrtRunOptions ->
  Int ->
  IO ()
ortApiRunOptionsSetRunLogVerbosityLevel options logVerbosityLevel = do
  ortApi <- getOrtApi options
  withOrtRunOptionsPtr options $ \optionsPtr -> do
    ortStatusPtr <-
      _wrap_OrtApi_RunOptionsSetRunLogVerbosityLevel
        optionsPtr
        (fromIntegral logVerbosityLevel)
    handleOrtStatus ortApi ortStatusPtr $ do
      pure ()

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_OrtApi_RunOptionsSetRunLogVerbosityLevel"
  _wrap_OrtApi_RunOptionsSetRunLogVerbosityLevel ::
    Ptr OrtRunOptions ->
    CInt ->
    IO (Ptr OrtStatus)

#{def
  OrtStatus* _wrap_OrtApi_RunOptionsSetRunLogVerbosityLevel(
    HsOrtRunOptions* options,
    int logVerbosityLevel
  ) {
    return options->ortApi->RunOptionsSetRunLogVerbosityLevel(
      options->ortRunOptions,
      logVerbosityLevel
    );
  }
}

-------------------------------------------------------------------------------
-- OrtApi::RunOptionsSetRunLogSeverityLevel

{- |
> ORT_API2_STATUS(RunOptionsSetRunLogSeverityLevel,
>   _Inout_ OrtRunOptions* options,
>   int log_severity_level
> );
-}
ortApiRunOptionsSetRunLogSeverityLevel ::
  OrtRunOptions ->
  Int ->
  IO ()
ortApiRunOptionsSetRunLogSeverityLevel options logSeverityLevel = do
  ortApi <- getOrtApi options
  withOrtRunOptionsPtr options $ \optionsPtr -> do
    ortStatusPtr <-
      _wrap_OrtApi_RunOptionsSetRunLogSeverityLevel
        optionsPtr
        (fromIntegral logSeverityLevel)
    handleOrtStatus ortApi ortStatusPtr $ do
      pure ()

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_OrtApi_RunOptionsSetRunLogSeverityLevel"
  _wrap_OrtApi_RunOptionsSetRunLogSeverityLevel ::
    Ptr OrtRunOptions ->
    CInt ->
    IO (Ptr OrtStatus)

#{def
  OrtStatus* _wrap_OrtApi_RunOptionsSetRunLogSeverityLevel(
    HsOrtRunOptions* options,
    int logSeverityLevel
  ) {
    return options->ortApi->RunOptionsSetRunLogSeverityLevel(
      options->ortRunOptions,
      logSeverityLevel
    );
  }
}

-------------------------------------------------------------------------------
-- OrtApi::RunOptionsSetRunTag

{- |
> ORT_API2_STATUS(RunOptionsSetRunTag,
>   _Inout_ OrtRunOptions* options,
>   _In_ const char* run_tag
> );
-}
ortApiRunOptionsSetRunTag ::
  OrtRunOptions ->
  String ->
  IO ()
ortApiRunOptionsSetRunTag options runTag = do
  ortApi <- getOrtApi options
  withCString runTag $ \runTagPtr -> do
    withOrtRunOptionsPtr options $ \optionsPtr -> do
      ortStatusPtr <-
        _wrap_OrtApi_RunOptionsSetRunTag
          optionsPtr
          (ConstPtr runTagPtr) -- NOTE: This is unsafe.
      handleOrtStatus ortApi ortStatusPtr $ do
        pure ()

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_OrtApi_RunOptionsSetRunTag"
  _wrap_OrtApi_RunOptionsSetRunTag ::
    Ptr OrtRunOptions ->
    ConstPtr CChar ->
    IO (Ptr OrtStatus)

#{def
  OrtStatus* _wrap_OrtApi_RunOptionsSetRunTag(
    HsOrtRunOptions* options,
    const char* runTag
  ) {
    return options->ortApi->RunOptionsSetRunTag(
      options->ortRunOptions,
      runTag
    );
  }
}

-------------------------------------------------------------------------------
-- OrtApi::RunOptionsGetRunLogVerbosityLevel

{- |
> ORT_API2_STATUS(RunOptionsGetRunLogVerbosityLevel,
>   _In_ const OrtRunOptions* options,
>   _Out_ int* log_verbosity_level
> );
-}
ortApiRunOptionsGetRunLogVerbosityLevel ::
  OrtRunOptions ->
  IO Int
ortApiRunOptionsGetRunLogVerbosityLevel options = do
  ortApi <- getOrtApi options
  alloca $ \outPtr -> do
    withOrtRunOptionsPtr options $ \optionsPtr -> do
      ortStatusPtr <-
        _wrap_OrtApi_RunOptionsGetRunLogVerbosityLevel
          optionsPtr
          outPtr
      handleOrtStatus ortApi ortStatusPtr $ do
        fromIntegral <$> peek outPtr

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_OrtApi_RunOptionsGetRunLogVerbosityLevel"
  _wrap_OrtApi_RunOptionsGetRunLogVerbosityLevel ::
    Ptr OrtRunOptions ->
    Ptr CInt ->
    IO (Ptr OrtStatus)

#{def
  OrtStatus* _wrap_OrtApi_RunOptionsGetRunLogVerbosityLevel(
    HsOrtRunOptions* options,
    int* logVerbosityLevel
  ) {
    return options->ortApi->RunOptionsGetRunLogVerbosityLevel(
      options->ortRunOptions,
      logVerbosityLevel
    );
  }
}

-------------------------------------------------------------------------------
-- OrtApi::RunOptionsGetRunLogSeverityLevel

{- |
> ORT_API2_STATUS(RunOptionsGetRunLogSeverityLevel,
>   _In_ const OrtRunOptions* options,
>   _Out_ int* log_severity_level
> );
-}
ortApiRunOptionsGetRunLogSeverityLevel ::
  OrtRunOptions ->
  IO Int
ortApiRunOptionsGetRunLogSeverityLevel options = do
  ortApi <- getOrtApi options
  alloca $ \outPtr -> do
    withOrtRunOptionsPtr options $ \optionsPtr -> do
      ortStatusPtr <-
        _wrap_OrtApi_RunOptionsGetRunLogSeverityLevel
          optionsPtr
          outPtr
      handleOrtStatus ortApi ortStatusPtr $ do
        fromIntegral <$> peek outPtr

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_OrtApi_RunOptionsGetRunLogSeverityLevel"
  _wrap_OrtApi_RunOptionsGetRunLogSeverityLevel ::
    Ptr OrtRunOptions ->
    Ptr CInt ->
    IO (Ptr OrtStatus)

#{def
  OrtStatus* _wrap_OrtApi_RunOptionsGetRunLogSeverityLevel(
    HsOrtRunOptions* options,
    int* logSeverityLevel
  ) {
    return options->ortApi->RunOptionsGetRunLogSeverityLevel(
      options->ortRunOptions,
      logSeverityLevel
    );
  }
}

-------------------------------------------------------------------------------
-- OrtApi::RunOptionsGetRunTag

{- |
> ORT_API2_STATUS(RunOptionsGetRunTag,
>   _In_ const OrtRunOptions* options,
>   _Out_ const char** run_tag
> );
-}
ortApiRunOptionsGetRunTag ::
  OrtRunOptions ->
  IO String
ortApiRunOptionsGetRunTag options = do
  ortApi <- getOrtApi options
  alloca @(ConstPtr CChar) $ \outPtr -> do
    withOrtRunOptionsPtr options $ \optionsPtr -> do
      ortStatusPtr <-
        _wrap_OrtApi_RunOptionsGetRunTag
          optionsPtr
          outPtr
      handleOrtStatus ortApi ortStatusPtr $ do
        peekCString . unConstPtr
          =<< peek outPtr

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_OrtApi_RunOptionsGetRunTag"
  _wrap_OrtApi_RunOptionsGetRunTag ::
    Ptr OrtRunOptions ->
    Ptr (ConstPtr CChar)  ->
    IO (Ptr OrtStatus)

#{def
  OrtStatus* _wrap_OrtApi_RunOptionsGetRunTag(
    HsOrtRunOptions* options,
    const char** runTag
  ) {
    return options->ortApi->RunOptionsGetRunTag(
      options->ortRunOptions,
      runTag
    );
  }
}

-------------------------------------------------------------------------------
-- OrtApi::RunOptionsSetTerminate

{- |
> ORT_API2_STATUS(RunOptionsSetTerminate,
>   _Inout_ OrtRunOptions* options
> );
-}
ortApiRunOptionsSetTerminate ::
  OrtRunOptions ->
  IO ()
ortApiRunOptionsSetTerminate options = do
  ortApi <- getOrtApi options
  withOrtRunOptionsPtr options $ \optionsPtr -> do
    ortStatusPtr <-
      _wrap_OrtApi_RunOptionsSetTerminate
        optionsPtr
    handleOrtStatus ortApi ortStatusPtr $ do
      pure ()

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_OrtApi_RunOptionsSetTerminate"
  _wrap_OrtApi_RunOptionsSetTerminate ::
    Ptr OrtRunOptions ->
    IO (Ptr OrtStatus)

#{def
  OrtStatus* _wrap_OrtApi_RunOptionsSetTerminate(
    HsOrtRunOptions* options
  ) {
    return options->ortApi->RunOptionsSetTerminate(
      options->ortRunOptions
    );
  }
}

-------------------------------------------------------------------------------
-- OrtApi::RunOptionsUnsetTerminate

{- |
> ORT_API2_STATUS(RunOptionsUnsetTerminate,
>   _Inout_ OrtRunOptions* options
> );
-}
ortApiRunOptionsUnsetTerminate ::
  OrtRunOptions ->
  IO ()
ortApiRunOptionsUnsetTerminate options = do
  ortApi <- getOrtApi options
  withOrtRunOptionsPtr options $ \optionsPtr -> do
    ortStatusPtr <-
      _wrap_OrtApi_RunOptionsUnsetTerminate
        optionsPtr
    handleOrtStatus ortApi ortStatusPtr $ do
      pure ()

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_OrtApi_RunOptionsUnsetTerminate"
  _wrap_OrtApi_RunOptionsUnsetTerminate ::
    Ptr OrtRunOptions ->
    IO (Ptr OrtStatus)

#{def
  OrtStatus* _wrap_OrtApi_RunOptionsUnsetTerminate(
    HsOrtRunOptions* options
  ) {
    return options->ortApi->RunOptionsUnsetTerminate(
      options->ortRunOptions
    );
  }
}

-------------------------------------------------------------------------------
-- OrtApi::CreateTensorAsOrtValue

-- TODO: Required to get OrtApi::Run working.

{-
> ORT_API2_STATUS(CreateTensorAsOrtValue,
>   _Inout_ OrtAllocator* allocator,
>   _In_ const int64_t* shape,
>   size_t shape_len,
>   ONNXTensorElementDataType type,
>   _Outptr_ OrtValue** out
> );
-}
ortApiCreateTensorAsOrtValue ::
  OrtAllocator ->
  [Int64] ->
  ONNXTensorElementDataType ->
  IO OrtValue
ortApiCreateTensorAsOrtValue allocator shape dataType = do
  ortApi <- getOrtApi allocator
  withCOrtAllocatorPtr allocator $ \cOrtAllocatorPtr -> do
    withArrayLen shape $ \shapeLen shapePtr -> do
      alloca $ \outPtr -> do
        ortStatusPtr <-
          _wrap_OrtApi_CreateTensorAsOrtValue
            ortApi.ortApiConstPtr
            cOrtAllocatorPtr
            shapePtr
            (fromIntegral shapeLen)
            dataType
            outPtr
        handleOrtStatus ortApi ortStatusPtr $ do
          wrapCOrtValue ortApi
            =<< peek outPtr

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_OrtApi_CreateTensorAsOrtValue"
  _wrap_OrtApi_CreateTensorAsOrtValue ::
    ConstPtr OrtApi ->
    Ptr COrtAllocator ->
    Ptr ( #{type int64_t} ) ->
    ( #{type size_t} ) ->
    ONNXTensorElementDataType ->
    Ptr (Ptr COrtValue) ->
    IO (Ptr OrtStatus)

#{def
  OrtStatus* _wrap_OrtApi_CreateTensorAsOrtValue(
    const OrtApi* ortApi,
    COrtAllocator* allocator,
    const int64_t* shape,
    size_t shapeLen,
    ONNXTensorElementDataType type,
    COrtValue** out
  ) {
    return ortApi->CreateTensorAsOrtValue(
      allocator,
      shape,
      shapeLen,
      type,
      out
    );
  }
}

-------------------------------------------------------------------------------
-- OrtApi::CreateTensorWithDataAsOrtValue

{-
> ORT_API2_STATUS(CreateTensorWithDataAsOrtValue,
>   _In_ const OrtMemoryInfo* info,
>   _Inout_ void* p_data,
>   size_t p_data_len,
>   _In_ const int64_t* shape,
>   size_t shape_len,
>   ONNXTensorElementDataType type,
>   _Outptr_ OrtValue** out
> );
-}

-------------------------------------------------------------------------------
-- OrtApi::IsTensor

{-
> ORT_API2_STATUS(IsTensor,
>   _In_ const OrtValue* value,
>   _Out_ int* out
> );
-}

-------------------------------------------------------------------------------
-- OrtApi::GetTensorMutableData

{-
> ORT_API2_STATUS(GetTensorMutableData,
>   _In_ OrtValue* value,
>   _Outptr_ void** out
> );
-}

-------------------------------------------------------------------------------
-- OrtApi::FillStringTensor

{-
> ORT_API2_STATUS(FillStringTensor,
>   _Inout_ OrtValue* value,
>   _In_ const char* const* s,
>   size_t s_len
> );
-}

-------------------------------------------------------------------------------
-- OrtApi::GetStringTensorDataLength

{-
> ORT_API2_STATUS(GetStringTensorDataLength,
>   _In_ const OrtValue* value,
>   _Out_ size_t* len
> );
-}

-------------------------------------------------------------------------------
-- OrtApi::GetStringTensorContent

{-
> ORT_API2_STATUS(GetStringTensorContent,
>   _In_ const OrtValue* value,
>   _Out_writes_bytes_all_(s_len) void* s,
>   size_t s_len,
>   _Out_writes_all_(offsets_len) size_t* offsets,
>   size_t offsets_len
> );
-}



-------------------------------------------------------------------------------
-- OrtApi::CastTypeInfoToTensorInfo

{-
> ORT_API2_STATUS(CastTypeInfoToTensorInfo,
>   _In_ const OrtTypeInfo* type_info,
>   _Outptr_result_maybenull_ const OrtTensorTypeAndShapeInfo** out
> );
-}

-------------------------------------------------------------------------------
-- OrtApi::GetOnnxTypeFromTypeInfo

{-
> ORT_API2_STATUS(GetOnnxTypeFromTypeInfo,
>   _In_ const OrtTypeInfo* type_info,
>   _Out_ enum ONNXType* out
> );
-}

-------------------------------------------------------------------------------
-- OrtApi::CreateTensorTypeAndShapeInfo

{-
> ORT_API2_STATUS(CreateTensorTypeAndShapeInfo,
>  _Outptr_ OrtTensorTypeAndShapeInfo** out
> );
-}

-------------------------------------------------------------------------------
-- OrtApi::SetTensorElementType

{-
> ORT_API2_STATUS(SetTensorElementType,
>  _Inout_ OrtTensorTypeAndShapeInfo* info,
>  enum ONNXTensorElementDataType type
> );
-}

-------------------------------------------------------------------------------
-- OrtApi::SetDimensions

{-
> ORT_API2_STATUS(SetDimensions,
>  OrtTensorTypeAndShapeInfo* info,
>  _In_ const int64_t* dim_values,
>  size_t dim_count
> );
-}

-------------------------------------------------------------------------------
-- OrtApi::GetTensorElementType

{-
> ORT_API2_STATUS(GetTensorElementType,
>  _In_ const OrtTensorTypeAndShapeInfo* info,
>  _Out_ enum ONNXTensorElementDataType* out
> );
-}

-------------------------------------------------------------------------------
-- OrtApi::GetDimensionsCount

{-
> ORT_API2_STATUS(GetDimensionsCount,
>  _In_ const OrtTensorTypeAndShapeInfo* info,
>  _Out_ size_t* out
> );
-}

-------------------------------------------------------------------------------
-- OrtApi::GetDimensions

{-
> ORT_API2_STATUS(GetDimensions,
>  _In_ const OrtTensorTypeAndShapeInfo* info,
>  _Out_ int64_t* dim_values,
>  size_t dim_values_length
> );
-}

-------------------------------------------------------------------------------
-- OrtApi::GetSymbolicDimensions

{-
> ORT_API2_STATUS(GetSymbolicDimensions,
>  _In_ const OrtTensorTypeAndShapeInfo* info,
>  _Out_writes_all_(dim_params_length) const char* dim_params[],
>  size_t dim_params_length
> );
-}

-------------------------------------------------------------------------------
-- OrtApi::GetTensorShapeElementCount

{-
> ORT_API2_STATUS(GetTensorShapeElementCount,
>  _In_ const OrtTensorTypeAndShapeInfo* info,
>  _Out_ size_t* out
> );
-}

-------------------------------------------------------------------------------
-- OrtApi::GetTensorTypeAndShape

{-
> ORT_API2_STATUS(GetTensorTypeAndShape,
>  _In_ const OrtValue* value,
>  _Outptr_ OrtTensorTypeAndShapeInfo** out
> );
-}

-------------------------------------------------------------------------------
-- OrtApi::GetTypeInfo

{-
> ORT_API2_STATUS(GetTypeInfo,
>  _In_ const OrtValue* value,
>  _Outptr_result_maybenull_ OrtTypeInfo** out
> );
-}

-------------------------------------------------------------------------------
-- OrtApi::GetValueType

{-
> ORT_API2_STATUS(GetValueType,
>  _In_ const OrtValue* value,
>  _Out_ enum ONNXType* out
> );
-}

-------------------------------------------------------------------------------
-- OrtApi::CreateMemoryInfo

{-
> ORT_API2_STATUS(CreateMemoryInfo,
>  _In_ const char* name,
>  enum OrtAllocatorType type,
>  int id,
>  enum OrtMemType mem_type,
>  _Outptr_ OrtMemoryInfo** out
> );
-}

-------------------------------------------------------------------------------
-- OrtApi::CreateCpuMemoryInfo

{-
> ORT_API2_STATUS(CreateCpuMemoryInfo,
>  enum OrtAllocatorType type,
>  enum OrtMemType mem_type,
>  _Outptr_ OrtMemoryInfo** out
> );
-}

-------------------------------------------------------------------------------
-- OrtApi::CompareMemoryInfo

{-
> ORT_API2_STATUS(CompareMemoryInfo,
>  _In_ const OrtMemoryInfo* info1,
>  _In_ const OrtMemoryInfo* info2,
>  _Out_ int* out
> );
-}

-------------------------------------------------------------------------------
-- OrtApi::MemoryInfoGetName

{-
> ORT_API2_STATUS(MemoryInfoGetName,
>  _In_ const OrtMemoryInfo* ptr,
>  _Out_ const char** out
> );
-}

-------------------------------------------------------------------------------
-- OrtApi::MemoryInfoGetId

{-
> ORT_API2_STATUS(MemoryInfoGetId,
>  _In_ const OrtMemoryInfo* ptr,
>  _Out_ int* out
> );
-}

-------------------------------------------------------------------------------
-- OrtApi::MemoryInfoGetMemType

{-
> ORT_API2_STATUS(MemoryInfoGetMemType,
>  _In_ const OrtMemoryInfo* ptr,
>  _Out_ OrtMemType* out
> );
-}

-------------------------------------------------------------------------------
-- OrtApi::MemoryInfoGetType

{-
> ORT_API2_STATUS(MemoryInfoGetType,
>  _In_ const OrtMemoryInfo* ptr,
>  _Out_ OrtAllocatorType* out
> );
-}

-------------------------------------------------------------------------------
-- OrtApi::AllocatorAlloc

{-
> ORT_API2_STATUS(AllocatorAlloc,
>  _Inout_ OrtAllocator* ort_allocator,
>  size_t size,
>  _Outptr_ void** out
> );
-}

-------------------------------------------------------------------------------
-- OrtApi::AllocatorFree

{-
> ORT_API2_STATUS(AllocatorFree,
>  _Inout_ OrtAllocator* ort_allocator,
>  void* p
> );
-}

-------------------------------------------------------------------------------
-- OrtApi::AllocatorGetInfo

{-
> ORT_API2_STATUS(AllocatorGetInfo,
>  _In_ const OrtAllocator* ort_allocator,
>  _Outptr_ const struct OrtMemoryInfo** out
> );
-}

-------------------------------------------------------------------------------
-- OrtApi::GetAllocatorWithDefaultOptions

{- |
> ORT_API2_STATUS(GetAllocatorWithDefaultOptions,
>  _Outptr_ OrtAllocator** out
> );
-}
ortApiGetAllocatorWithDefaultOptions ::
  OrtApi ->
  IO OrtAllocator
ortApiGetAllocatorWithDefaultOptions ortApi = do
  alloca $ \outPtr -> do
    ortStatusPtr <-
      _wrap_OrtApi_GetAllocatorWithDefaultOptions
        ortApi.ortApiConstPtr
        outPtr
    handleOrtStatus ortApi ortStatusPtr $ do
      wrapCOrtAllocator ortApi
        =<< peek outPtr

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_OrtApi_GetAllocatorWithDefaultOptions"
  _wrap_OrtApi_GetAllocatorWithDefaultOptions ::
    ConstPtr OrtApi ->
    Ptr (Ptr COrtAllocator) ->
    IO (Ptr OrtStatus)

#{def
  OrtStatus* _wrap_OrtApi_GetAllocatorWithDefaultOptions(
    const OrtApi* ortApi,
    OrtAllocator** out
  ) {
    return ortApi->GetAllocatorWithDefaultOptions(out);
  }
}

-------------------------------------------------------------------------------
-- OrtApi::AddFreeDimensionOverride

{- |
> ORT_API2_STATUS(AddFreeDimensionOverride,
>  _Inout_ OrtSessionOptions* options,
>  _In_ const char* dim_denotation,
>  _In_ int64_t dim_value
> );
-}
ortApiAddFreeDimensionOverride ::
  OrtSessionOptions ->
  String ->
  Int ->
  IO ()
ortApiAddFreeDimensionOverride options dimDenotation dimValue = do
  ortApi <- getOrtApi options
  withCString dimDenotation $ \dimDenotationPtr ->
    withOrtSessionOptionsPtr options $ \optionsPtr -> do
      ortStatusPtr <-
        _wrap_OrtApi_AddFreeDimensionOverride
          optionsPtr
          (ConstPtr dimDenotationPtr) -- NOTE: This is unsafe.
          (fromIntegral dimValue)
      handleOrtStatus ortApi ortStatusPtr $ do
        pure ()

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h _wrap_OrtApi_AddFreeDimensionOverride"
  _wrap_OrtApi_AddFreeDimensionOverride ::
    Ptr OrtSessionOptions ->
    ConstPtr CChar ->
    ( #{type int64_t} ) ->
    IO (Ptr OrtStatus)

#{def
  OrtStatus* _wrap_OrtApi_AddFreeDimensionOverride(
    HsOrtSessionOptions* options,
    const char* dimDenotation,
    int64_t dimValue
  ) {
    return options->ortApi->AddFreeDimensionOverride(
      options->ortSessionOptions,
      dimDenotation,
      dimValue
    );
  }
}

-------------------------------------------------------------------------------
-- OrtApi::GetValue

{-
> ORT_API2_STATUS(GetValue,
>   _In_ const OrtValue* value,
>   int index,
>   _Inout_ OrtAllocator* allocator,
>   _Outptr_ OrtValue** out
> );
-}

-------------------------------------------------------------------------------
-- OrtApi::GetValueCount

{-
> ORT_API2_STATUS(GetValueCount,
>   _In_ const OrtValue* value,
>   _Out_ size_t* out
> );
-}

-------------------------------------------------------------------------------
-- OrtApi::CreateValue

{-
> ORT_API2_STATUS(CreateValue,
>   _In_reads_(num_values) const OrtValue* const* in,
>   size_t num_values,
>   enum ONNXType value_type,
>   _Outptr_ OrtValue** out
> );
-}

-------------------------------------------------------------------------------
-- OrtApi::CreateOpaqueValue

{-
> ORT_API2_STATUS(CreateOpaqueValue,
>   _In_z_ const char* domain_name,
>   _In_z_ const char* type_name,
>   _In_ const void* data_container,
>   size_t data_container_size,
>   _Outptr_ OrtValue** out
> );
-}

-------------------------------------------------------------------------------
-- OrtApi::GetOpaqueValue

{-
> ORT_API2_STATUS(GetOpaqueValue,
>   _In_ const char* domain_name,
>   _In_ const char* type_name,
>   _In_ const OrtValue* in,
>   _Out_ void* data_container,
>   size_t data_container_size
> );
-}

-------------------------------------------------------------------------------
-- OrtApi::KernelInfoGetAttribute_float

-- ORT_API2_STATUS(KernelInfoGetAttribute_float, _In_ const OrtKernelInfo* info, _In_ const char* name, _Out_ float* out);

-------------------------------------------------------------------------------
-- OrtApi::KernelInfoGetAttribute_int64

-- ORT_API2_STATUS(KernelInfoGetAttribute_int64, _In_ const OrtKernelInfo* info, _In_ const char* name, _Out_ int64_t* out);

-------------------------------------------------------------------------------
-- OrtApi::KernelInfoGetAttribute_string

-- ORT_API2_STATUS(KernelInfoGetAttribute_string, _In_ const OrtKernelInfo* info, _In_ const char* name, _Out_ char* out, _Inout_ size_t* size);

-------------------------------------------------------------------------------
-- OrtApi::KernelContext_GetInputCount

-- ORT_API2_STATUS(KernelContext_GetInputCount, _In_ const OrtKernelContext* context, _Out_ size_t* out);

-------------------------------------------------------------------------------
-- OrtApi::KernelContext_GetOutputCount

-- ORT_API2_STATUS(KernelContext_GetOutputCount, _In_ const OrtKernelContext* context, _Out_ size_t* out);

-------------------------------------------------------------------------------
-- OrtApi::KernelContext_GetInput

-- ORT_API2_STATUS(KernelContext_GetInput, _In_ const OrtKernelContext* context, _In_ size_t index, _Out_ const OrtValue** out);

-------------------------------------------------------------------------------
-- OrtApi::KernelContext_GetOutput

-- ORT_API2_STATUS(KernelContext_GetOutput, _Inout_ OrtKernelContext* context, _In_ size_t index, _In_ const int64_t* dim_values, size_t dim_count, _Outptr_ OrtValue** out);

-------------------------------------------------------------------------------
-- OrtApi::GetDenotationFromTypeInfo

{-
> ORT_API2_STATUS(GetDenotationFromTypeInfo,
>   _In_ const OrtTypeInfo* type_info,
>   _Out_ const char** const denotation,
>   _Out_ size_t* len
> );
-}

-------------------------------------------------------------------------------
-- OrtApi::CastTypeInfoToMapTypeInfo

{-
> ORT_API2_STATUS(CastTypeInfoToMapTypeInfo,
>   _In_ const OrtTypeInfo* type_info,
>   _Outptr_result_maybenull_ const OrtMapTypeInfo** out
> );
-}

-------------------------------------------------------------------------------
-- OrtApi::CastTypeInfoToSequenceTypeInfo

{-
> ORT_API2_STATUS(CastTypeInfoToSequenceTypeInfo,
>   _In_ const OrtTypeInfo* type_info,
>   _Outptr_result_maybenull_ const OrtSequenceTypeInfo** out
> );
-}

-- ... and many more
