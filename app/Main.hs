import OnnxRuntime

main :: IO ()
main = do
  ortApiBase <- c_OrtGetApiBase
  print ortApiBase
