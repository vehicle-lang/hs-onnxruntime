{-# LANGUAGE ForeignFunctionInterface #-}

module Vehicle.OnnxRuntime where

#include <vehicle_onnxruntime.h>

import Foreign
import Foreign.C.Types
import Foreign.C.String

getInt :: CInt
getInt = {#call pure get_int #}