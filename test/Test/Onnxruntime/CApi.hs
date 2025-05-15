{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Onnxruntime.CApi where

import Data.List qualified as L
import Data.Vector.Storable (Vector)
import Data.Vector.Storable qualified as VS
import Data.Version (Version, makeVersion, parseVersion)
import Onnxruntime.CApi
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, assertBool, testCase, (@?), (@?=))
import Text.ParserCombinators.ReadP (readP_to_S)

tests :: TestTree
tests =
  testGroup
    "CApi"
    [ test_ortApiBaseGetVersionString
    , test_ortApiRun
    ]

test_ortApiBaseGetVersionString :: TestTree
test_ortApiBaseGetVersionString =
  testCase "test_ortApiBaseGetVersionString" $ do
    ortApiBase <- ortGetApiBase
    versionString <- ortApiBaseGetVersionString ortApiBase
    case readVersion versionString of
      Nothing -> assertFailure $ "Could not parse version " <> versionString
      Just version -> version >= makeVersion [1, 21] @? "Onnxruntime version is <1.21"

test_ortApiRun :: TestTree
test_ortApiRun = do
  testCase "test_ortApiRun" $ do
    -- Get OrtApi
    ortApiBase <- ortGetApiBase
    ortApi <- ortApiBaseGetApi ortApiBase ortApiVersion
    -- Create OrtEnv
    let logid = "test_ortApiRun"
    ortEnv <- ortApiCreateEnv ortApi OrtLoggingLevelFatal logid
    -- Create OrtSessionOptions
    ortSessionOptions <- ortApiCreateSessionOptions ortApi
    -- Create OrtSession
    -- TODO: make model path more robust
    let modelPath = "test/data/controller.onnx"
    ortSession <- ortApiCreateSession ortEnv modelPath ortSessionOptions
    -- Create OrtRunOptions
    ortRunOptions <- ortApiCreateRunOptions ortApi
    -- Create OrtMemoryInfo
    ortMemoryInfo <- ortApiCreateCpuMemoryInfo ortApi OrtDeviceAllocator OrtMemTypeDefault
    -- Create input OrtValue
    let input1Data = VS.fromList [10,10] :: Vector Float
    ortApiWithTensorWithDataAsOrtValue ortMemoryInfo input1Data [1,2] $ \input1 -> do
      -- Run
      let inputNames = ["input_1"]
      let outputNames = ["dense_3"]
      outputs <- ortApiRun ortSession ortRunOptions inputNames [input1] outputNames
      assertBool "there is one output" (length outputs == 1)
      let [output] = outputs
      outputIsTensor <- ortApiIsTensor output
      assertBool "the output is tensor" outputIsTensor
      outputTypeAndShape <- ortApiGetTensorTypeAndShape output
      outputDimensions <- ortApiGetDimensions outputTypeAndShape
      outputDimensions @?= [1, 1]
      outputElementCount <- ortApiGetTensorShapeElementCount outputTypeAndShape
      outputElementCount @?= 1
      ortApiWithTensorData output $ \(outputData :: Vector Float) -> do
        VS.length outputData @?= 1
        outputData VS.! 0 @?= -76

-- | Internal helper. Read a 'Version' from a 'String'.
readVersion :: String -> Maybe Version
readVersion = fmap fst . L.find (null . snd) . readP_to_S parseVersion
