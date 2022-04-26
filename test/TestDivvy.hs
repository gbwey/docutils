{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestDivvy where

import Data.Char
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import DocUtils.Divvy
import Test.Tasty
import Test.Tasty.HUnit

suite :: TestTree
suite =
  testGroup
    "TestDivvy"
    [ testCase "divvyKeyed" $
        divvyKeyed id id [('a', 100 :: Int), ('b', 200), ('x', 11)] [('c', 12), ('b', 201), ('x', 11)]
          @?= ( Set.fromList [('a', 100)]
              , Set.fromList [('c', 12)]
              , M.fromList
                  [ (LT, (('b', 200), ('b', 201)) :| [])
                  , (EQ, (('x', 11), ('x', 11)) :| [])
                  ]
              )
    , testCase "divvyKeyed" $
        divvyKeyed id toLower [('a', 100 :: Int), ('b', 200), ('x', 11)] [('C', 12), ('B', 201), ('X', 11)]
          @?= ( Set.fromList [('a', 100)]
              , Set.fromList [('C', 12)]
              , M.fromList
                  [ (LT, (('b', 200), ('B', 201)) :| [])
                  , (EQ, (('x', 11), ('X', 11)) :| [])
                  ]
              )
    , testCase "divvyKeyed" $
        divvyKeyed id id [('a', 100 :: Int), ('b', 200), ('x', 11)] [('C', 12), ('B', 201), ('X', 11)]
          @?= ( Set.fromList [('a', 100), ('b', 200), ('x', 11)]
              , Set.fromList [('B', 201), ('C', 12), ('X', 11)]
              , M.empty
              )
    ]
