{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.OnnxRuntime where

import Control.Monad (forM)
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class (MonadIO(..))
import Control.Exception (throwIO, Exception)

import Foreign.C.String
import Foreign.Ptr (Ptr)

import Vehicle.OnnxRuntime.Bindings
import Foreign (free)

newtype OnnxRuntime a = OnnxRuntime (ResourceT IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

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

getNetworkType :: Session -> OnnxRuntime [()]
getNetworkType (Session sessionPtr) = OnnxRuntime $ do
  inputSize <- liftIO $ onnxruntimeHS_SessionGetInputCount sessionPtr
  forM [0..(fromIntegral inputSize - 1)] $ \inputIndex -> do
    let getTypeInfo = onnxruntimeHS_SessionGetInputTypeInfo sessionPtr (fromIntegral inputIndex)
    (_releaseKey, typeInfo) <- allocate getTypeInfo onnxruntimeHS_ReleaseTypeInfo
    tensorTypeAndShapeInfo <- liftIO $ onnxruntimeHS_CastTypeInfoToTensorInfo typeInfo
    return ()
