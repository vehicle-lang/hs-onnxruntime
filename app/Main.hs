import Vehicle.OnnxRuntime
import Foreign.C.String

main :: IO ()
main = do
  putStrLn =<< getVersion

  networkType <- runOnnxRuntime $ do
    env <- createEnv
    sessionOptions <- createSessionOptions

    -- let network = "/home/matthew/Code/AISEC/vehicle/examples/windController/controller.onnx"
    --let network = "/home/matthew/Code/AISEC/vehicle/test/networks/identity-2.onnx"
    let network = "/home/matthew/Code/AISEC/Marabou/resources/onnx/acasxu/ACASXU_experimental_v2a_1_1.onnx"
    session <- createSession env sessionOptions network

    getNetworkType session

  print networkType