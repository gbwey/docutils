{-# LANGUAGE OverloadedStrings #-}

module TestTime where

import DocUtils.Time
import Test.Tasty
import Test.Tasty.HUnit

doit :: IO ()
doit = defaultMain suite

suite :: TestTree
suite =
  testGroup
    "TestTime"
    [ testCase "toseconds" $ toSeconds (HHMMSSDD 1 0 0 99) @?= 3600
    , testCase "toseconds" $ toSeconds (HHMMSSDD 1 0 0 120) @?= 3601
    , testCase "to100seconds" $ to100Seconds (HHMMSSDD 1 0 0 3) @?= 360003
    ]
