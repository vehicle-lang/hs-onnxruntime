module Main where

import Test.Onnxruntime.CApi qualified as CApi
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
    defaultMain $
        testGroup
            "Onnxruntime"
            [ CApi.tests
            ]
