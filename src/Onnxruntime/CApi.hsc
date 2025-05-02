{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Onnxruntime.CApi where

import Data.Kind (Type)
import Data.Coerce (coerce)
import Foreign
import Foreign.C.ConstPtr.Compat (ConstPtr (..))
import Foreign.C.Types
import Foreign.C.String
import GHC.TypeLits (Natural)

#include <onnxruntime_c_api.h>

-------------------------------------------------------------------------------
-- OrtApiVersion
-------------------------------------------------------------------------------

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
-------------------------------------------------------------------------------

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
-- OrtLoggingLevel
-------------------------------------------------------------------------------

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
-- OrtErrorCode
-------------------------------------------------------------------------------

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
-- OrtApi
-------------------------------------------------------------------------------

newtype
    {-# CTYPE "onnxruntime_c_api.h" "OrtApi" #-}
    OrtApi = OrtApi { ortApiConstPtr :: ConstPtr OrtApi }

-------------------------------------------------------------------------------
-- OrtStatus
-------------------------------------------------------------------------------

data
  {-# CTYPE "onnxruntime_c_api.h" "COrtStatus" #-}
  COrtStatus

#{def
  typedef OrtStatus COrtStatus;
}

newtype
  {-# CTYPE "Onnxruntime/CApi_hsc.h" "HsOrtStatus" #-}
  OrtStatus = OrtStatus { ortStatusForeignPtr :: ForeignPtr OrtStatus }

#{def
  typedef struct HsOrtStatus {
    const OrtApi* ortApi;
    COrtStatus* ortStatus;
  } HsOrtStatus;
}

withOrtStatusPtr ::
  OrtStatus ->
  (Ptr OrtStatus -> IO a) ->
  IO a
withOrtStatusPtr ortStatus =
  withForeignPtr ortStatus.ortStatusForeignPtr

wrapCOrtStatus ::
  OrtApi ->
  Ptr COrtStatus ->
  IO OrtStatus
wrapCOrtStatus ortApi rawOrtStatusPtr = do
  ortStatusPtr <- _wrap_COrtStatus ortApi.ortApiConstPtr rawOrtStatusPtr
  ortStatusForeignPtr <- newForeignPtr _wrap_OrtApi_ReleaseStatus ortStatusPtr
  pure $ OrtStatus ortStatusForeignPtr

foreign import capi unsafe
    "Onnxruntime/CApi_hsc.h _wrap_COrtStatus"
    _wrap_COrtStatus ::
        ConstPtr OrtApi ->
        Ptr COrtStatus ->
        IO (Ptr OrtStatus)

#{def
  HsOrtStatus* _wrap_COrtStatus(
    const OrtApi* ortApi,
    COrtStatus* ortStatus
  ) {
    HsOrtStatus *out = malloc(sizeof *out);
    out->ortApi = ortApi;
    out->ortStatus = ortStatus;
    return out;
  }
}

foreign import capi unsafe
  "Onnxruntime/CApi_hsc.h &_wrap_OrtApi_ReleaseStatus"
  _wrap_OrtApi_ReleaseStatus ::
    FunPtr (
      Ptr OrtStatus ->
      IO ()
    )

#{def
  void _wrap_OrtApi_ReleaseStatus(HsOrtStatus* ortStatus) {
    ortStatus->ortApi->ReleaseStatus(ortStatus->ortStatus);
    free(ortStatus);
  }
}

-------------------------------------------------------------------------------
-- OrtApi::CreateStatus

ortApiCreateStatus ::
  OrtApi ->
  OrtErrorCode ->
  String ->
  IO OrtStatus
ortApiCreateStatus ortApi code msg = do
  withCString msg $ \msgPtr -> do
    wrapCOrtStatus ortApi
      =<< _wrap_OrtApi_CreateStatus ortApi.ortApiConstPtr code (ConstPtr msgPtr)

foreign import capi unsafe
    "Onnxruntime/CApi_hsc.h _wrap_OrtApi_CreateStatus"
    _wrap_OrtApi_CreateStatus ::
        ConstPtr OrtApi ->
        OrtErrorCode ->
        ConstPtr CChar ->
        IO (Ptr COrtStatus)

#{def
    COrtStatus* _wrap_OrtApi_CreateStatus(
      const OrtApi* ortApi,
      OrtErrorCode code,
      const char* msg
    ) {
        return ortApi->CreateStatus(code, msg);
    }
}

-------------------------------------------------------------------------------
-- OrtApi::GetErrorCode

ortApiGetErrorCode ::
  OrtStatus ->
  IO OrtErrorCode
ortApiGetErrorCode ortStatus =
  withOrtStatusPtr ortStatus _wrap_OrtApi_GetErrorCode

foreign import capi unsafe
    "Onnxruntime/CApi_hsc.h _wrap_OrtApi_GetErrorCode"
    _wrap_OrtApi_GetErrorCode ::
        Ptr OrtStatus ->
        IO OrtErrorCode

#{def
    OrtErrorCode _wrap_OrtApi_GetErrorCode(
      HsOrtStatus* ortStatus
    ) {
        return ortStatus->ortApi->GetErrorCode(ortStatus->ortStatus);
    }
}

-------------------------------------------------------------------------------
-- OrtApi::GetErrorMessage

ortApiGetErrorMessage ::
  OrtStatus ->
  IO String
ortApiGetErrorMessage ortStatus =
  withOrtStatusPtr ortStatus $ \ortStatusPtr -> do
    ConstPtr msgPtr <- _wrap_OrtApi_GetErrorMessage ortStatusPtr
    peekCString msgPtr

foreign import capi unsafe
    "Onnxruntime/CApi_hsc.h _wrap_OrtApi_GetErrorMessage"
    _wrap_OrtApi_GetErrorMessage ::
        Ptr OrtStatus ->
        IO (ConstPtr CChar)

#{def
    const char* _wrap_OrtApi_GetErrorMessage(
      HsOrtStatus* ortStatus
    ) {
        return ortStatus->ortApi->GetErrorMessage(ortStatus->ortStatus);
    }
}


-------------------------------------------------------------------------------
-- OrtEnv
-------------------------------------------------------------------------------

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

withOrtEnvPtr ::
  OrtEnv ->
  (Ptr OrtEnv -> IO a) ->
  IO a
withOrtEnvPtr ortEnv =
  withForeignPtr ortEnv.ortEnvForeignPtr

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
