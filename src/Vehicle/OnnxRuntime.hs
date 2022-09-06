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

getNetworkType :: Session -> OnnxRuntime ([[Int]], [[Int]])
getNetworkType session = do
  inputSize <- getInputCount session
  inputTypes <- forM [0.. inputSize-1] $ \inputIndex -> do
    (inputReleaseKey, inputInfo) <- getInputTensorTypeAndShapeInfo session inputIndex
    inputDimensions <- getDimensions inputInfo
    release inputReleaseKey
    return inputDimensions

  outputSize <- getOutputCount session
  outputTypes <- forM [0.. outputSize-1] $ \outputIndex -> do
    (outputReleaseKey, outputInfo) <- getOutputTensorTypeAndShapeInfo session outputIndex
    outputDimensions <- getDimensions outputInfo
    release outputReleaseKey
    return outputDimensions

  return (inputTypes, outputTypes)

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