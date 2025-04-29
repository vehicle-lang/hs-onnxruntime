module Main where

import Test.Tasty (defaultMain, testGroup)
import Test.Onnxruntime.CApi qualified as CApi

main :: IO ()
main = do
  defaultMain $
    testGroup "Onnxruntime" [
        CApi.tests
    ]
