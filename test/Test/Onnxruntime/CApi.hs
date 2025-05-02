module Test.Onnxruntime.CApi where

import Data.List qualified as L
import Data.Version (Version, makeVersion, parseVersion)
import Onnxruntime.CApi
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?), (@=?))
import Text.ParserCombinators.ReadP (readP_to_S)

readVersion :: String -> Maybe Version
readVersion = fmap fst . L.find (null . snd) . readP_to_S parseVersion

tests :: TestTree
tests =
  testGroup
    "CApi"
    [ test_ortApiBaseGetVersionString
    , test_ortApiOrtStatus
    ]

test_ortApiBaseGetVersionString :: TestTree
test_ortApiBaseGetVersionString =
  testCase "test_ortApiBaseGetVersionString" $ do
    ortApiBase <- ortGetApiBase
    versionString <- ortApiBaseGetVersionString ortApiBase
    case readVersion versionString of
      Nothing -> assertFailure $ "Could not parse version " <> versionString
      Just version -> version >= makeVersion [1, 21] @? "Onnxruntime version is <1.21"

test_ortApiOrtStatus :: TestTree
test_ortApiOrtStatus =
  testCase "test_ortApiOrtStatus" $ do
    ortApiBase <- ortGetApiBase
    ortApi <- ortApiBaseGetApi ortApiBase ortApiVersion

    let inputMsg = "Just hunky-dory"
    ortStatus <- ortApiCreateStatus ortApi OrtOk inputMsg
    code <- ortApiGetErrorCode ortStatus
    msg <- ortApiGetErrorMessage ortStatus

    OrtOk @=? code 
    inputMsg @=? msg
