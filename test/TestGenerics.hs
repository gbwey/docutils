{-# OPTIONS -Wno-partial-fields #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-- max of 8-tuple for generic-lens so if you have more than that then stuff fails
--   so just need to nest it: hence the weird nesting in the tests
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module TestGenerics where

import Control.Lens
import Data.Coerce
import Data.Generics.Product
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as N
import Data.Ratio
import Data.These
import Data.Time
import Data.Time.Clock.POSIX
import Data.Typeable
import DocUtils.Generics
import GHC.Generics
import qualified GHC.Generics as G
import Test.Tasty
import Test.Tasty.HUnit

zt1, zt2, zt3, zt4 :: ZonedTimeG ()
zt1 = ZonedTimeG $ ZonedTime (read "2021-01-01 12:14:19") (TimeZone 341 True "aBc")
zt2 = ZonedTimeG $ read "2019-12-23 08:02:57 EST"
zt3 = ZonedTimeG $ read "1900-11-12 23:18:52 MST"
zt4 = ZonedTimeG $ read "1912-10-11 22:53:41 UT"

ut1, ut2, ut3, ut4 :: UTCTimeG ()
ut1 = UTCTimeG $ read "2012-08-12 19:25:02 CST"
ut2 = UTCTimeG $ read "1999-11-11 18:58:42 PST"
ut3 = UTCTimeG $ read "1998-11-11 18:58:42 MST"
ut4 = UTCTimeG $ read "1997-11-11 18:58:42 UT"

lt1, lt2, lt3, lt4 :: LocalTimeG ()
lt1 = LocalTimeG $ zonedTimeToLocalTime $ read "2021-02-02 13:15:20 MST"
lt2 = LocalTimeG $ zonedTimeToLocalTime $ read "2022-03-03 14:16:21 MST"
lt3 = LocalTimeG $ zonedTimeToLocalTime $ read "2023-04-05 12:18:59 MST"
lt4 = LocalTimeG $ zonedTimeToLocalTime $ read "2024-05-06 11:12:08 MST"

nt1, nt2, nt3, nt4 :: NominalDiffTimeG ()
nt1 = NominalDiffTimeG $ utcTimeToPOSIXSeconds $ read "2014-08-12 21:25:02 PDT"
nt2 = NominalDiffTimeG $ utcTimeToPOSIXSeconds $ read "2012-09-12 19:29:17 CST"
nt3 = NominalDiffTimeG $ utcTimeToPOSIXSeconds $ read "2015-10-30 22:26:02 PST"
nt4 = NominalDiffTimeG $ utcTimeToPOSIXSeconds $ read "2016-11-29 21:25:02 PST"

dt1, dt2 :: DiffTimeG ()
dt1 = DiffTimeG $ fromRational (toRational nt3)
dt2 = DiffTimeG $ fromRational (toRational nt4)

dy1, dy2 :: DayG ()
dy1 = DayG $ utctDay $ read "2009-12-31 04:15:05 CST"
dy2 = DayG $ utctDay $ read "2008-11-30 05:22:52 UT"

td1, td2 :: TimeOfDayG ()
td1 = TimeOfDayG $ localTimeOfDay $ coerce lt3
td2 = TimeOfDayG $ localTimeOfDay $ coerce lt4

rt1, rt2 :: RatioG () Int
rt1 = RatioG (12 % 13)
rt2 = RatioG (3 % (-12))

rt1', rt2' :: RationalG ()
rt1' = RatioG ((-15) % 3)
rt2' = RatioG (2 % 99)

doit :: IO ()
doit = defaultMain suite

suite :: TestTree
suite = testGroup "TestGenerics" [suite0, suite1, suite2]

suite0 :: TestTree
suite0 =
  testGroup
    "generic datetime wrappers"
    [ testCase "UTCTimeG" $ G.to @(UTCTimeG ()) (G.from ut1) @?= ut1
    , testCase "ZonedTimeG" $ G.to @(ZonedTimeG ()) (G.from zt1) @?= zt1
    , testCase "LocalTimeG" $ G.to @(LocalTimeG ()) (G.from lt1) @?= lt1
    , testCase "DiffTimeG" $ G.to @(DiffTimeG ()) (G.from dt1) @?= dt1
    , testCase "NominalDiffTimeG" $ G.to @(NominalDiffTimeG ()) (G.from nt1) @?= nt1
    , testCase "DayG" $ G.to @(DayG ()) (G.from dy1) @?= dy1
    , testCase "TimeOfDayG" $ G.to @(TimeOfDayG ()) (G.from td1) @?= td1
    , testCase "RatioG Int" $ G.to @(RatioG () Int) (G.from rt1) @?= rt1
    , testCase "RatioG Integer" $ G.to @(RatioG () Integer) (G.from rt1') @?= rt1'
    , testCase "generic lens types ()" $ (zt1, ut1, lt1, nt1, dt1, dy1, (td1, rt1, ())) ^.. types @() @?= [()] -- max of 8-tuple for generic-lens so have to nest the tuples
    , testCase "generic lens types Int" $ (zt1, ut1, lt1, nt1, dt1, dy1, (td1, rt1, rt1', ())) ^.. types @Int @?= [341, 12, 13]
    , testCase "generic lens types Integer" $ (zt1, ut1, lt1, nt1, dt1, dy1, (td1, rt1, rt1', ())) ^.. types @Integer @?= [-5, 1] -- looking for leaks
    , testCase "generic lens types RationalG" $ (zt1, ut1, lt1, nt1, dt1, dy1, (td1, rt1, rt1', ())) ^.. types @(RationalG ()) @?= [RatioG ((-5) % 1)] -- looking for leaks
    , testCase "generic lens types RatioG Int" $ (zt1, ut1, lt1, nt1, dt1, dy1, (td1, rt1, rt1', ())) ^.. types @(RatioG () Int) @?= [RatioG (12 % 13)] -- looking for leaks
    , testCase "generic lens types String" $ (zt1, ut1, lt1, nt1, dt1, dy1, (td1, rt1, ())) ^.. types @String @?= ["aBc"]
    , testCase "generic lens types Bool" $ (zt1, ut1, lt1, nt1, dt1, dy1, (td1, rt1, ())) ^.. types @Bool @?= [True]
    , testCase "generic lens types DayG" $ (zt1, ut1, lt1, nt1, dt1, dy1, (td1, rt1, ())) ^.. types @(DayG ()) @?= [DayG $ fromGregorian 2009 12 31]
    , testCase "getSize" $ getSize @(These () ()) @?= 3
    , testCase "getStaticField" $ getStaticField @(These Int Char) @?= (("This", [("", intT)]) :| [("That", [("", charT)]), ("These", [("", intT), ("", charT)])])
    , testCase "getValueField" $ getValueField (This 2 :: These Int Char) @?= (ValueFields "This" [ValueField "" intT "2"] :| [])
    , testCase "getValueField" $ getValueField (That 'x' :: These Int Char) @?= (ValueFields "That" [ValueField "" charT "'x'"] :| [])
    , testCase "conNames" $ conNames (That 'x') @?= ("This" :| ["That", "These"])
    , testCase "conNameOf" $ conNameOf (That 'x') @?= "That"
    , testCase "getStaticField" $ getStaticField @BRecordI @?= (("BRecord", [("fieldB1", typeRep (Proxy @(Maybe String))), ("fieldB2", intT)]) :| [])
    , testCase "getStaticField" $ getStaticField @(LR Char Int) @?= (("L1", [("", charT)]) :| [("L2", [("", charT), ("", intT)]), ("L3", [("", charT)])])
    ]

charT :: TypeRep
charT = typeRep (Proxy @Char)

intT :: TypeRep
intT = typeRep (Proxy @Int)

data ARecordI = ARecord
  { fieldA1 :: Maybe String
  , fieldA2 :: Maybe Int
  , fieldA3 :: Bool
  , fieldA4 :: Int
  , fieldA5 :: (Int, Maybe Char)
  , fieldA6 :: BRecordI
  , fieldA7 :: Maybe BRecordI
  }
  deriving stock (Generic, Show)

data BRecordI = BRecord
  { fieldB1 :: Maybe String
  , fieldB2 :: Int
  }
  deriving stock (Generic, Show)

data LR a b
  = L1 a
  | L2 a b
  | L3 a
  deriving stock (Show, Generic)

data ELR a b
  = ENone
  | ELeft {eleftOnly :: a}
  | ERight {erightOnly :: b}
  | EBoth {eleft :: a, eright :: b}
  deriving stock (Show, Generic)

data Three a b c
  = T0
  | T1 {t1a :: a, t1b :: b, t1c :: c}
  | T2 {t2a :: a, t2b :: b, t2c :: c}
  | T3 {t3a :: a, t3b :: b, t3c :: c}
  deriving stock (Show, Generic)

suite1 :: TestTree
suite1 =
  testGroup
    "ValueField"
    [ testCase "simple record" $
        getValueField
          ( BRecord
              (Just "abc")
              999
          )
          @?= pure
            ( ValueFields
                "BRecord"
                [ ValueField "fieldB1" (tp @(Maybe String)) "Just \"abc\""
                , ValueField "fieldB2" (tp @Int) "999"
                ]
            )
    , testCase "complex record" $
        getValueField
          ( ARecord
              (Just "abc")
              Nothing
              True
              123
              (987, Just 'x')
              (BRecord Nothing 12)
              (Just (BRecord (Just "x") 123))
          )
          @?= pure
            ( ValueFields
                "ARecord"
                [ ValueField "fieldA1" (tp @(Maybe String)) "Just \"abc\""
                , ValueField "fieldA2" (tp @(Maybe Int)) "Nothing"
                , ValueField "fieldA3" (tp @Bool) "True"
                , ValueField "fieldA4" (tp @Int) "123"
                , ValueField "fieldA5" (tp @(Int, Maybe Char)) "(987,Just 'x')"
                , ValueField "fieldA6" (tp @BRecordI) "BRecord {fieldB1 = Nothing, fieldB2 = 12}"
                , ValueField "fieldA7" (tp @(Maybe BRecordI)) "Just (BRecord {fieldB1 = Just \"x\", fieldB2 = 123})"
                ]
            )
    , testCase "simple sum type" $
        getValueField (Just 'x')
          @?= pure (ValueFields "Just" [ValueField "" (tp @Char) "'x'"])
    , testCase "string" $
        getValueField ("asf" :: String)
          @?= pure
            ( ValueFields
                ":"
                [ ValueField "" (tp @Char) "'a'"
                , ValueField "" (tp @String) "\"sf\""
                ]
            )
    , testCase "empty string" $
        getValueField ("" :: String)
          @?= pure (ValueFields "[]" [])
    , testCase "nothing" $
        getValueField (Nothing @Int)
          @?= pure (ValueFields "Nothing" [])
    ]

suite2 :: TestTree
suite2 =
  testGroup
    "Static"
    [ testCase "sum type" $
        genericStaticField @(Rep (LR Char Int))
          @?= N.fromList
            [ ("L1", [("", tp @Char)])
            , ("L2", [("", tp @Char), ("", tp @Int)])
            , ("L3", [("", tp @Char)])
            ]
    , testCase "sum type2" $
        genericStaticField @(Rep (ELR Char Int))
          @?= N.fromList
            [ ("ENone", []) -- ENone
            , ("ELeft", [("eleftOnly", tp @Char)])
            , ("ERight", [("erightOnly", tp @Int)])
            , ("EBoth", [("eleft", tp @Char), ("eright", tp @Int)])
            ]
    , testCase "simple sum type3" $
        genericStaticField @(Rep (Three Char Int Bool))
          @?= N.fromList
            [ ("T0", [])
            , ("T1", [("t1a", tp @Char), ("t1b", tp @Int), ("t1c", tp @Bool)])
            , ("T2", [("t2a", tp @Char), ("t2b", tp @Int), ("t2c", tp @Bool)])
            , ("T3", [("t3a", tp @Char), ("t3b", tp @Int), ("t3c", tp @Bool)])
            ]
    ]

tp :: forall a. Typeable a => TypeRep
tp = typeRep (Proxy @a)
