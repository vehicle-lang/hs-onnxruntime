import OnnxRuntime
import Foreign.C.String

main :: IO ()
main = do
  ortApiBase <- getOrtApiBase
  version <- getVersion ortApiBase
  putStrLn version