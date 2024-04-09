module Main (main) where

import Text.Luatemp

import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString, findByExtension)

import System.FilePath (dropExtension, takeBaseName)

main :: IO ()
main = defaultMain =<< goldenTests

goldenTests :: IO TestTree
goldenTests = do
  expecteds <- findByExtension [".expected"] "test-files"
  pure $ testGroup "golden tests"
    [ goldenVsString
        (takeBaseName sourceFile)
        expectedFile
        (runLuaFile sourceFile)
    | expectedFile <- expecteds
    , let sourceFile = dropExtension expectedFile
    ]