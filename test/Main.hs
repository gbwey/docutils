module Main where

import Test.Tasty
import qualified TestCondition
import qualified TestDivvy
import qualified TestDoc
import qualified TestGenerics
import qualified TestParser
import qualified TestTime

main :: IO ()
main = do
  defaultMain $
    testGroup
      "alltests"
      [ TestCondition.suite
      , TestDivvy.suite
      , TestDoc.suite
      , TestGenerics.suite
      , TestParser.suite
      , TestTime.suite
      ]
