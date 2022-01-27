import Onnx.Runtime
import Foreign.C.String

main :: IO ()
main = do
  version <- ortVersion
  putStrLn version