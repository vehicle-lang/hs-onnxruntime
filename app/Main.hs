import Onnx.Runtime
import Foreign.C.String

main :: IO ()
main = do
  createOrtEnv
  version <- ortVersion
  putStrLn version