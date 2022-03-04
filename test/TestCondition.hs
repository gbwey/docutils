{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module TestCondition where

import Data.List.NonEmpty (NonEmpty (..))
import DocUtils.Condition
import Test.Tasty
import Test.Tasty.HUnit
import Validation

doit :: IO ()
doit = defaultMain suite

suite :: TestTree
suite =
  testGroup
    "TestCondition"
    [ testCase "expectOneM >1" $ expectVFailureWith ["length=3"] (pure ExpectOneM) (expectOneM @Integer Post "x" [1, 2, 3]) @?= Right ()
    , testCase "expectOneM =0" $ expectOneM @Int Post "x" [] @?= Success Nothing
    , testCase "expectOneM =1" $ expectOneM @Int Post "x" [12] @?= Success (Just 12)
    , testCase "expectOne >1" $ expectVFailureWith ["length=3"] (pure ExpectOne) (expectOne @Integer Post "x" [1, 2, 3]) @?= Right ()
    , testCase "expectOne =0" $ expectVFailureWith ["found none"] (pure ExpectOne) (expectOne @Int Post "x" []) @?= Right ()
    , testCase "expectOne =1" $ expectOne @Int Post "x" [12] @?= Success 12
    , testCase "expectUniqueM >1" $ expectVFailureWith ["length=2"] (pure ExpectUniqueM) (expectUniqueM @Int Pre "x" [1, 1, 1, 0]) @?= Right ()
    , testCase "expectUniqueM >1" $ expectVFailureWith ["length=3"] (pure ExpectUniqueM) (expectUniqueM @Int Pre "x" [1, 2, 3]) @?= Right ()
    , testCase "expectUniqueM =1" $ expectUniqueM @Int Pre "x" [1, 1, 1] @?= Success (Just 1)
    , testCase "expectUniqueM =0" $ expectUniqueM @Int Pre "x" [] @?= Success Nothing
    , testCase "expectUniqueM =1" $ expectUniqueM @Int Pre "x" [1] @?= Success (Just 1)
    , testCase "expectUnique =0" $ expectVFailureWith ["found none"] (pure ExpectUnique) (expectUnique @Int Pre "x" []) @?= Right ()
    , testCase "expectUnique >1" $ expectVFailureWith ["length=3"] (pure ExpectUnique) (expectUnique @Int Pre "x" [1, 2, 3]) @?= Right ()
    , testCase "expectUnique =1" $ expectUnique @Int Pre "x" [1, 1, 1] @?= Success 1
    , testCase "expectUnique =1" $ expectUnique @Int Pre "x" [1] @?= Success 1
    , testCase "expectnodups bad" $ expectVFailureWith ["length=1"] (pure ExpectNoDups) (expectNoDups @Int Pre "x" [1, 1]) @?= Right ()
    , testCase "expectnodups bad" $ expectVFailureWith ["length=3"] (pure ExpectNoDups) (expectNoDups @Int Pre "x" [1, 5, 2, 3, 1, 4, 4, 4, 5]) @?= Right ()
    , testCase "expectnodups ok" $ expectNoDups @Int Pre "x" [1, 2, 5, 0] @?= Success [1, 2, 5, 0]
    , testCase "expectnodups ok" $ expectNoDups @Int Pre "x" [1] @?= Success [1]
    , testCase "expectnodups ok" $ expectNoDups @Int Pre "x" [] @?= Success []
    , testCase "expectnull not empty" $ expectVFailureWith ["length=1"] (pure ExpectEmpty) (expectEmpty @Int Pre "x" [1]) @?= Right ()
    , testCase "expectnull empty" $ expectEmpty @Int Pre "x" [] @?= Success ()
    , testCase "expectnotnull empty" $ expectVFailureWith mempty (pure ExpectNonEmpty) (expectNonEmpty @Int Pre "x" []) @?= Right ()
    , testCase "expectnotnull has values" $ expectNonEmpty Pre "x" ['a', 'b'] @?= Success ('a' :| ['b'])
    , testCase "expecteq nope" $ expectVFailureWith mempty (pure ExpectEq) (expectEq Pre "x" 'a' 'b') @?= Right ()
    , testCase "expecteq yes" $ expectEq Pre "x" 'a' 'a' @?= Success ()
    , testCase "is ascending 0" $ expectAscending @Int 0 Pre "x" [0] @?= Success [0]
    , testCase "is ascending" $ expectAscending @Int 0 Pre "x" [0 .. 10] @?= Success [0 .. 10]
    , testCase "is not ascending" $ expectVFailureWith mempty (pure ExpectAscending) (expectAscending @Int 0 Pre "x" ([0 .. 10] ++ [10])) @?= Right ()
    , testCase "is not ascending starts at 1 thru 10" $ expectVFailureWith mempty (pure ExpectAscending) (expectAscending @Int 0 Pre "x" [1 .. 10]) @?= Right ()
    , testCase "is not ascending starts at 1" $ expectVFailureWith mempty (pure ExpectAscending) (expectAscending @Int 0 Pre "x" [1]) @?= Right ()
    ]
