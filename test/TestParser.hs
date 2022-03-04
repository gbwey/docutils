{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module TestParser where

import Control.Arrow
import Data.Either
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.These
import Data.Time
import DocUtils.Parser
import DocUtils.Time
import System.Time.Extra
import Test.Tasty
import Test.Tasty.HUnit
import qualified Text.Megaparsec as Z
import qualified Text.Megaparsec.Char as Z
import Utils.Parser
import Utils.Positive

doit :: IO ()
doit = defaultMain suite

suite :: TestTree
suite =
  testGroup
    "TestParser"
    [ testCase "duration empty" $ expectLeftWithT (pure "expecting some data") (runP parseDurationP "") @?= Right ()
    , testCase "duration 1hour" $ runP parseDurationP "1h" @?= Right (HHMMSSDD 1 0 0 0)
    , testCase "duration 150s" $ runP parseDurationP "150.4s" @?= Right (HHMMSSDD 0 0 150 40)
    , testCase "duration 3s" $ runP parseDurationP "0h3s" @?= Right (HHMMSSDD 0 0 3 0)
    , testCase "duration 2m" $ runP parseDurationP "2m" @?= Right (HHMMSSDD 0 2 0 0)
    , testCase "duration bad" $ expectLeftWithT (pure "expecting '.', 's', or digit") (runP parseDurationP "10") @?= Right ()
    , testCase "range" $ expectLeftWithT (pure "invalid digits") (runP (rangeP @Integer "testtimeutils" _2P (4, 10)) "x") @?= Right ()
    , testCase "range" $ expectLeftWithT (pure "invalid digits") (runP (rangeP @Integer "testtimeutils" _2P (4, 10)) "xyzzz") @?= Right ()
    , testCase "range" $ expectLeftWithT (pure "end of input") (runP (rangeP @Integer "testtimeutils" _2P (4, 10)) "7") @?= Right ()
    , testCase "range" $ expectLeftWithT (pure "not in range") (runP (rangeP @Integer "testtimeutils" _1P (4, 10)) "11") @?= Right ()
    , testCase "range" $ expectLeftWithT (pure "start is greater than end") (runP (rangeP @Integer "testtimeutils" _11P (11, 10)) "y") @?= Right ()
    , testCase "range" $ runP (rangeP @Integer "testtimeutils" _2P (4, 10)) "06" @?= Right 6
    , testCase "range" $ runP (rangeP @Integer "testtimeutils" _2P (6, 6)) "06" @?= Right 6
    , testCase "range" $ runP (rangeP @Integer "testtimeutils" _1P (6, 6)) "6" @?= Right 6
    , testCase "utctime" $ runP (utcTimeP "testtimeutils") "20111201_121314" @?= Right (toUtcTime' (2011, 12, 1) (12, 13, 14))
    , testCase "utctime month=13" $ expectLeftWithT (pure "13 not in range") (runP (utcTimeP "testtimeutils") "20111301_121314") @?= Right ()
    , testCase "parseDurationP" $ runP parseDurationP (T.pack (showDuration 60)) @?= Right (HHMMSSDD 0 1 0 0)
    , testCase "parseDurationP" $ runP parseDurationP (T.pack (showDuration 60.9)) @?= Right (HHMMSSDD 0 1 1 0)
    , testCase "parseDurationP" $ runP parseDurationP (T.pack (showDuration 60.99)) @?= Right (HHMMSSDD 0 1 1 0)
    , testCase "parseDurationP" $ runP parseDurationP (T.pack (showDuration 60.56)) @?= Right (HHMMSSDD 0 1 1 0)
    , testCase "parseDurationP" $ runP parseDurationP (T.pack (showDuration 50.56)) @?= Right (HHMMSSDD 0 0 50 56)
    , testCase "parseIntRangeP" $ runP parseIntRangeP "2-3" @?= Right (2 :| [3])
    , testCase "parseIntRangeP" $ runP parseIntRangeP "12" @?= Right (12 :| [])
    , testCase "parseIntRangeP" $ runP parseIntRangeP "1-13" @?= Right (1 :| [2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13])
    , testCase "parseIntRangeP" $ runP parseIntRangeP "4..6" @?= Right (4 :| [5, 6])
    , testCase "parseIntRangeP" $ runP parseIntRangeP "4..4" @?= Right (4 :| [])
    , testCase "parseIntRangeP" $ assertBool "Af" $ isLeft $ runP parseIntRangeP "4-2"
    , testCase "parseIntRanges" $ parseIntRanges "12 4 1-3 10..13" @?= Right ((12 :| []) :| [4 :| [], 1 :| [2, 3], 10 :| [11, 12, 13]])
    , testCase "chkDay" $ chkDay "20211230" @?= Right (fromGregorian 2021 12 30)
    , testCase "theseP" $ runP (theseP "xx" (Z.some Z.digitChar)) "xx" @?= Right (This "xx")
    , testCase "theseP" $ runP (theseP "xx" (Z.some Z.digitChar)) "123" @?= Right (That "123")
    , testCase "theseP" $ runP (theseP "xx" (Z.some Z.digitChar)) "12zz3" @?= Right (That "12")
    , testCase "theseP" $ runP (theseP "xx" (Z.some Z.digitChar)) "12 xx" @?= Right (That "12")
    , testCase "theseP" $ runP (theseP "xx" (Z.some Z.digitChar)) "12xx" @?= Right (These "xx" "12")
    ]

expectLeftWithT :: Show a => NonEmpty String -> Either Text a -> Either String ()
expectLeftWithT xs = expectLeftWith xs . left T.unpack

-- | expect a 'Left' with the text matching infix with "ns"
expectLeftWith :: Show a => NonEmpty String -> Either String a -> Either String ()
expectLeftWith _ (Right a) = Left $ "expected fail but was actually successful " ++ show a
expectLeftWith ns (Left s)
  | all (`L.isInfixOf` s) ns = Right ()
  | otherwise = Left $ "found fail but infix string did not match: actual[" ++ s ++ "] infix[" ++ show ns ++ "]"
