{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestDoc where

import DocUtils.Doc
import Test.Tasty
import Test.Tasty.HUnit

suite :: TestTree
suite =
  testGroup
    "TestDoc"
    [ testCase "pSepEmpty" $ show (pSepEmpty "-" ["abc", "def", mempty, "ggg"]) @?= "abc-def-ggg"
    ]
