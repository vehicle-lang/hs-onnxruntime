import Vehicle.OnnxRuntime
import Foreign.C.String

main :: IO ()
main = do
  let version = getInt
  print version