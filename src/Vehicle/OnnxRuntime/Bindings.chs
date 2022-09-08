{-# LANGUAGE ForeignFunctionInterface #-}

module Vehicle.OnnxRuntime.Bindings where

import Foreign
import Foreign.C.Types
import Foreign.C.String

#include <vehicle_onnxruntime.h>

{#enum ONNXTensorElementDataType
  as OnnxElementType
  {underscoreToCase}
  with prefix = "ONNX_TENSOR_ELEMENT_DATA_TYPE_"
  add  prefix = "ONNX_"
  deriving (Eq, Show)
  #}

newtype Env = Env (Ptr Env)
newtype SessionOptions = SessionOptions (Ptr SessionOptions)
newtype Session = Session (Ptr Session)
newtype TypeInfo = TypeInfo (Ptr TypeInfo)
newtype TensorTypeAndShapeInfo = TensorTypeAndShapeInfo (Ptr TensorTypeAndShapeInfo)

foreign import ccall unsafe "vehicle_onnxruntime.h" onnxruntimeHS_Init :: IO CInt
foreign import ccall unsafe "vehicle_onnxruntime.h" onnxruntimeHS_GetVersionString :: IO CString

foreign import ccall unsafe "vehicle_onnxruntime.h" onnxruntimeHS_CreateEnv :: IO (Ptr Env)
foreign import ccall unsafe "vehicle_onnxruntime.h" onnxruntimeHS_ReleaseEnv :: Ptr Env -> IO ()

foreign import ccall unsafe "vehicle_onnxruntime.h" onnxruntimeHS_CreateSessionOptions :: IO (Ptr SessionOptions)
foreign import ccall unsafe "vehicle_onnxruntime.h" onnxruntimeHS_ReleaseSessionOptions :: Ptr SessionOptions -> IO ()

foreign import ccall unsafe "vehicle_onnxruntime.h" onnxruntimeHS_CreateSession :: Ptr Env -> Ptr SessionOptions -> CString -> IO (Ptr Session)
foreign import ccall unsafe "vehicle_onnxruntime.h" onnxruntimeHS_ReleaseSession :: Ptr Session -> IO ()

foreign import ccall unsafe "vehicle_onnxruntime.h" onnxruntimeHS_SessionGetInputCount :: Ptr Session -> IO CSize
foreign import ccall unsafe "vehicle_onnxruntime.h" onnxruntimeHS_SessionGetInputTypeInfo :: Ptr Session -> CSize -> IO (Ptr TypeInfo)

foreign import ccall unsafe "vehicle_onnxruntime.h" onnxruntimeHS_SessionGetOutputCount :: Ptr Session -> IO CSize
foreign import ccall unsafe "vehicle_onnxruntime.h" onnxruntimeHS_SessionGetOutputTypeInfo :: Ptr Session -> CSize -> IO (Ptr TypeInfo)

foreign import ccall unsafe "vehicle_onnxruntime.h" onnxruntimeHS_ReleaseTypeInfo :: Ptr TypeInfo -> IO ()
foreign import ccall unsafe "vehicle_onnxruntime.h" onnxruntimeHS_CastTypeInfoToTensorInfo :: Ptr TypeInfo -> IO (Ptr TensorTypeAndShapeInfo)

foreign import ccall unsafe "vehicle_onnxruntime.h" onnxruntimeHS_GetDimensionsCount :: Ptr TensorTypeAndShapeInfo -> IO CSize
foreign import ccall unsafe "vehicle_onnxruntime.h" onnxruntimeHS_GetDimensions :: Ptr TensorTypeAndShapeInfo -> Ptr CLong -> CSize -> IO ()
foreign import ccall unsafe "vehicle_onnxruntime.h" onnxruntimeHS_GetTensorElementType :: Ptr TensorTypeAndShapeInfo -> IO CInt