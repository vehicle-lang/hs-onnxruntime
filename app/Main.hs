import Vehicle.OnnxRuntime
import Foreign.C.String

main :: IO ()
main = do
  putStrLn =<< getVersion

  networkType <- runOnnxRuntime $ do
    env <- createEnv
    sessionOptions <- createSessionOptions
    session <- createSession env sessionOptions "/home/matthew/Code/AISEC/vehicle/examples/windController/controller.onnx"

    getNetworkType session

  print networkType