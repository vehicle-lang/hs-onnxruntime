{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Onnxruntime.CApi where

import Data.Kind (Type)
import Data.Coerce (coerce)
import Foreign
import Foreign.C.ConstPtr.Compat (ConstPtr (..))
import Foreign.C.Types
import Foreign.C.String
import GHC.TypeLits (Natural)

#include <onnxruntime_c_api.h>

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

{- |
The helper interface to get the right version of 'OrtApi'.

Get a pointer to this structure through 'ortGetApiBase'.
-}
newtype
    {-# CTYPE "onnxruntime_c_api.h" "OrtApiBase" #-}
    OrtApiBase = OrtApiBase { ortApiBaseConstPtr :: ConstPtr OrtApiBase }

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

newtype
    {-# CTYPE "onnxruntime_c_api.h" "OrtApi" #-}
    OrtApi = OrtApi { ortApiConstPtr :: ConstPtr OrtApi }

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
