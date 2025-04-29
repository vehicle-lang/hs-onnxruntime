module Test.Onnxruntime.CApi where

import Data.List qualified as L
import Data.Version (Version, makeVersion, parseVersion)
import Text.ParserCombinators.ReadP (readP_to_S)
import Onnxruntime.CApi
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?))

readVersion :: String -> Maybe Version
readVersion = fmap fst . L.find (null . snd) . readP_to_S parseVersion

tests :: TestTree
tests =
    testGroup "CApi"
        [ test_ortApiBaseGetVersionString
        ]

test_ortApiBaseGetVersionString :: TestTree
test_ortApiBaseGetVersionString =
    testCase "test_ortApiBaseGetVersionString" $ do
        ortApiBase <- ortGetApiBase
        versionString <- ortApiBaseGetVersionString ortApiBase
        case readVersion versionString of
            Nothing -> assertFailure $ "Could not parse version " <> versionString
            Just version -> version >= makeVersion [1,21] @? "Onnxruntime version is <1.21"
