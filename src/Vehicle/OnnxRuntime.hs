{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.OnnxRuntime where

import Control.Monad (forM)
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class (MonadIO(..))
import Control.Exception (throwIO, Exception)

import Foreign.C.String
import Foreign.Ptr (Ptr)

import Vehicle.OnnxRuntime.Bindings
import Foreign (free, mallocArray, newArray, peekArray)

newtype OnnxRuntime a = OnnxRuntime (ResourceT IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadResource)

data OnnxRuntimeError =
  OnnxRuntimeInitFail
  deriving (Show)

instance Exception OnnxRuntimeError

runOnnxRuntime :: OnnxRuntime a -> IO a
runOnnxRuntime (OnnxRuntime action) = do
  status <- onnxruntimeHS_Init
  if status == 0
    then runResourceT action
    else throwIO OnnxRuntimeInitFail

getVersion :: IO String
getVersion = peekCString =<< onnxruntimeHS_GetVersionString

createEnv :: OnnxRuntime Env
createEnv = OnnxRuntime $ do
  (_releaseKey, envPtr) <- allocate onnxruntimeHS_CreateEnv onnxruntimeHS_ReleaseEnv
  return $ Env envPtr

createSessionOptions :: OnnxRuntime SessionOptions
createSessionOptions = OnnxRuntime $ do
  (_releaseKey, sessionOptionsPtr) <-
    allocate onnxruntimeHS_CreateSessionOptions onnxruntimeHS_ReleaseSessionOptions
  return $ SessionOptions sessionOptionsPtr

createSession :: Env -> SessionOptions -> FilePath -> OnnxRuntime Session
createSession (Env envPtr) (SessionOptions sessionOptionsPtr) modelPath = OnnxRuntime $ do
  -- If we do get SEGFAULTS, might be because allocate calls release function when the
  -- release key drops out of scope.
  (_releaseKey, model_path_str) <- allocate (newCString modelPath) free
  let create_session = onnxruntimeHS_CreateSession envPtr sessionOptionsPtr model_path_str

  (_releaseKey, sessionPtr) <- allocate create_session onnxruntimeHS_ReleaseSession
  return $ Session sessionPtr

data OnnxNetworkType = OnnxNetworkType
  { inputTypes  :: [OnnxTensorType]
  , outputTypes :: [OnnxTensorType]
  } deriving (Show)

data OnnxTensorType = OnnxTensorType
  { elementType :: OnnxElementType
  , dimensions  :: [Int]
  } deriving (Show)

getNetworkType :: Session -> OnnxRuntime OnnxNetworkType
getNetworkType session = do
  inputTypes <- getTensorTypes session getInputCount getInputTensorTypeAndShapeInfo
  outputTypes <- getTensorTypes session getOutputCount getOutputTensorTypeAndShapeInfo
  return $ OnnxNetworkType inputTypes outputTypes

getTensorTypes :: Session
               -> (Session -> OnnxRuntime Int)
               -> (Session -> Int -> OnnxRuntime (ReleaseKey, TensorTypeAndShapeInfo))
               -> OnnxRuntime [OnnxTensorType]
getTensorTypes session getCount getTensorTypeAndShapeInfo = do
  count <- getCount session
  forM [0.. count-1] $ \index ->
    getTensorType session getTensorTypeAndShapeInfo index

getTensorType :: Session
              -> (Session -> Int -> OnnxRuntime (ReleaseKey, TensorTypeAndShapeInfo))
              -> Int
              -> OnnxRuntime OnnxTensorType
getTensorType session getTensorTypeAndShapeInfo index = do
  (releaseKey, tensorInfo) <- getTensorTypeAndShapeInfo session index
  elementType <- getElementType tensorInfo
  dimensions <- getDimensions tensorInfo
  release releaseKey
  return $ OnnxTensorType elementType dimensions

getInputCount :: Session -> OnnxRuntime Int
getInputCount (Session sessionPtr) = do
  inputCount <- liftIO $ onnxruntimeHS_SessionGetInputCount sessionPtr
  return $ fromIntegral inputCount

getInputTensorTypeAndShapeInfo :: Session -> Int -> OnnxRuntime (ReleaseKey, TensorTypeAndShapeInfo)
getInputTensorTypeAndShapeInfo (Session sessionPtr) inputIndex = do
  let getTypeInfo = onnxruntimeHS_SessionGetInputTypeInfo sessionPtr (fromIntegral inputIndex)
  (releaseKey, typeInfoPtr) <- allocate getTypeInfo onnxruntimeHS_ReleaseTypeInfo
  tensorTypeAndShapeInfoPtr <- liftIO $ onnxruntimeHS_CastTypeInfoToTensorInfo typeInfoPtr
  return (releaseKey, TensorTypeAndShapeInfo tensorTypeAndShapeInfoPtr)

getOutputCount :: Session -> OnnxRuntime Int
getOutputCount (Session sessionPtr) = do
  outputCount <- liftIO $ onnxruntimeHS_SessionGetOutputCount sessionPtr
  return $ fromIntegral outputCount

getOutputTensorTypeAndShapeInfo :: Session -> Int -> OnnxRuntime (ReleaseKey, TensorTypeAndShapeInfo)
getOutputTensorTypeAndShapeInfo (Session sessionPtr) outputIndex = do
  let getTypeInfo = onnxruntimeHS_SessionGetOutputTypeInfo sessionPtr (fromIntegral outputIndex)
  (releaseKey, typeInfoPtr) <- allocate getTypeInfo onnxruntimeHS_ReleaseTypeInfo
  tensorTypeAndShapeInfoPtr <- liftIO $ onnxruntimeHS_CastTypeInfoToTensorInfo typeInfoPtr
  return (releaseKey, TensorTypeAndShapeInfo tensorTypeAndShapeInfoPtr)

getDimensions :: TensorTypeAndShapeInfo -> OnnxRuntime [Int]
getDimensions (TensorTypeAndShapeInfo tensorInfo) = do
  dimensionsCSize <- liftIO $ onnxruntimeHS_GetDimensionsCount tensorInfo
  let dimensionsSize = fromIntegral dimensionsCSize

  let newDimensionsArray = newArray $ replicate dimensionsSize 0

  (releaseKey, dimensionsPtr) <- allocate newDimensionsArray free
  liftIO $ onnxruntimeHS_GetDimensions tensorInfo dimensionsPtr dimensionsCSize
  dimensions <- liftIO $ peekArray dimensionsSize dimensionsPtr
  release releaseKey

  return $ fmap fromIntegral dimensions

getElementType :: TensorTypeAndShapeInfo -> OnnxRuntime OnnxElementType
getElementType (TensorTypeAndShapeInfo tensorInfo) = do
  elementType <- liftIO $ onnxruntimeHS_GetTensorElementType tensorInfo
  return $ toEnum (fromIntegral elementType)