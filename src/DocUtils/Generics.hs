{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : DocUtils.Generics
Description : generic methods: eg for getting constructor names and values
            and generic instances for wrapped time values
Copyright   : (c) Grant Weyburne, 2022
License     : BSD-3
-}
module DocUtils.Generics (
  HasStaticField (..),
  -- GetIndex'(..),
  ConNames (..),
  HasValueField (..),
  getValueField,
  filterValueField,
  getStaticField,
  splitRational,
  joinRational,
  localTimeToPair,
  pairToLocalTime,
  getIndex,
  getSize,
  conNames,
  conNameOf,
  ValueFields (..),
  ValueField (..),
  UTCTimeG (..),
  LocalTimeG (..),
  ZonedTimeG (..),
  DayG (..),
  NominalDiffTimeG (..),
  DiffTimeG (..),
  TimeOfDayG (..),
  RatioG (..),
  RationalG,
  RationalT,
  RatioT,
  UTCTimeG',
  LocalTimeG',
  ZonedTimeG',
  DayG',
  NominalDiffTimeG',
  DiffTimeG',
  TimeOfDayG',
  RatioG',
  pattern M3K,
) where

import Control.Arrow
import Data.Generics.Product
import Data.Kind
import Data.Ratio
import Data.Time
import qualified Data.Time.Format.Internal as TI
import Data.Typeable
import DocUtils.Time
import GHC.Generics
import Primus.Error (normalError)

import Data.List.NonEmpty (NonEmpty (..))

-- shallow one level down

-- | ADT holding the constructor name and each of the fields in 'ValueField'
data ValueFields = ValueFields
  { fdsConName :: String -- cant be strict as has error in a between state
  , fdsFields :: ![ValueField]
  }
  deriving stock (Generic, Show, Eq)

-- | ADT containing the fieldname, type, and value of a field
data ValueField = ValueField
  { fdFieldName :: !String
  , fdTypeRep :: !TypeRep
  , fdShowString :: !String
  }
  deriving stock (Generic, Show, Eq)

-- | extracts the constructor name and the fields types and values from "a"
getValueField :: (HasValueField (Rep a), Generic a) => a -> NonEmpty ValueFields
getValueField x = genericValueField (from x)

-- | predicate on the 'Just' constructor for 'ValueField'
filterValueField :: ValueField -> Bool
filterValueField fld =
  take 5 (fdShowString fld) == "Just " -- supreme hack
    && typeRepTyCon (typeOf (Just 'x')) == typeRepTyCon (fdTypeRep fld) -- check if a maybe

-- | class for pulling out fields from a constructor
type HasValueField :: (Type -> Type) -> Constraint
class HasValueField f where
  genericValueField :: f x -> NonEmpty ValueFields

instance HasValueField f => HasValueField (D1 c f) where
  genericValueField (M1 x) = genericValueField x

instance (HasValueField x, HasValueField y) => HasValueField (x :+: y) where
  genericValueField (L1 l) = genericValueField l
  genericValueField (R1 r) = genericValueField r

instance (HasValueField x, HasValueField y) => HasValueField (x :*: y) where
  genericValueField (l :*: r) = genericValueField l <> genericValueField r

instance (HasValueField f, Constructor c) => HasValueField (C1 c f) where
  genericValueField z@(M1 x) =
    pure (ValueFields (conName z) (concatMap fdsFields (genericValueField x)))

instance (Typeable c', Show c', Selector c) => HasValueField (S1 c (K1 i c')) where
  genericValueField z@(M1 (K1 x)) = pure (ValueFields (normalError "S1") [ValueField (selName z) (typeOf x) (show x)])

instance HasValueField U1 where -- called for empty constructors eg getValueField (Nothing @Int)
  genericValueField U1 = pure (ValueFields (normalError "U1") [])

-- | extracts the static fields from a generic type
getStaticField :: forall a. HasStaticField (Rep a) => NonEmpty (String, [(String, TypeRep)])
getStaticField = genericStaticField @(Rep a)

-- | similar to 'HasValueField' but for static data
type HasStaticField :: (Type -> Type) -> Constraint
class HasStaticField f where
  genericStaticField :: NonEmpty (String, [(String, TypeRep)])

instance HasStaticField f => HasStaticField (D1 c f) where
  genericStaticField = genericStaticField @f

instance (HasStaticField x, HasStaticField y) => HasStaticField (x :+: y) where
  genericStaticField = genericStaticField @x <> genericStaticField @y

instance (HasStaticField x, HasStaticField y) => HasStaticField (x :*: y) where
  genericStaticField = genericStaticField @x <> genericStaticField @y

instance (Constructor c, HasStaticField f) => HasStaticField (C1 c f) where
  genericStaticField = pure (conName @c (M1 (Proxy @f)), concatMap snd (genericStaticField @f))

instance (Typeable c', Selector c) => HasStaticField (S1 c (K1 i c')) where
  genericStaticField = pure (normalError "S1", [(selName @c (normalError "should be fine"), typeRep (Proxy @c'))])

instance HasStaticField U1 where -- called for empty constructors
  genericStaticField = pure (normalError "U1", [])

-- | wrapper for 'UTCTime'
newtype UTCTimeG p = UTCTimeG UTCTime
  deriving (Eq, Show, Ord, Read, FormatTime) via UTCTime

instance ParseTime (UTCTimeG p) where
  parseTimeSpecifier _ = TI.parseTimeSpecifier (Proxy @UTCTime)
  buildTime l xs = UTCTimeG <$> TI.buildTime l xs

-- | generic instance for 'UTCTimeG'
instance Generic (UTCTimeG p) where
  type
    Rep (UTCTimeG p) =
      D1
        ( 'MetaData "UTCTimeG" "Main" "main" 'True)
        ( C1
            ( 'MetaCons "UTCTimeG" 'PrefixI 'False)
            ( S1
                ( 'MetaSel
                    'Nothing
                    'NoSourceUnpackedness
                    'NoSourceStrictness
                    'DecidedLazy
                )
                (Rec0 (Param 0 Integer))
            )
        )
  from (UTCTimeG u) = M3K (StarParam (utcToInteger u))
  to (M3K (StarParam secs)) = UTCTimeG (picoSecondsToUtc secs)

-- | wrapper for 'LocalTime'
newtype LocalTimeG p = LocalTimeG LocalTime
  deriving (Eq, Show, Ord, Read, FormatTime) via LocalTime

instance ParseTime (LocalTimeG p) where
  parseTimeSpecifier _ = TI.parseTimeSpecifier (Proxy @LocalTime)
  buildTime l xs = LocalTimeG <$> TI.buildTime l xs

-- | generic instance for 'LocalTimeG'
instance Generic (LocalTimeG p) where
  type
    Rep (LocalTimeG p) =
      D1
        ( 'MetaData "LocalTimeG" "Main" "main" 'True)
        ( C1
            ( 'MetaCons "LocalTimeG" 'PrefixI 'False)
            ( S1
                ( 'MetaSel
                    'Nothing
                    'NoSourceUnpackedness
                    'NoSourceStrictness
                    'DecidedLazy
                )
                (Rec0 (Param 0 (Integer, RationalT)))
            )
        )
  from (LocalTimeG u) = M3K (StarParam (localTimeToPair u))
  to (M3K (StarParam secs)) = LocalTimeG (pairToLocalTime secs)

-- | wrapper for 'ZonedTime'
newtype ZonedTimeG p = ZonedTimeG ZonedTime
  deriving (Show, Read, FormatTime) via ZonedTime

instance ParseTime (ZonedTimeG p) where
  parseTimeSpecifier _ = TI.parseTimeSpecifier (Proxy @ZonedTime)
  buildTime l xs = ZonedTimeG <$> TI.buildTime l xs

-- Ord makes no sense but we can do field equality for us
instance Eq (ZonedTimeG p) where
  ZonedTimeG (ZonedTime a b) == ZonedTimeG (ZonedTime a' b') =
    (a, b) == (a', b')

-- | generic instance for 'ZonedTimeG'
instance Generic (ZonedTimeG p) where
  type
    Rep (ZonedTimeG p) =
      D1
        ( 'MetaData "ZonedTimeG" "Main" "main" 'True)
        ( C1
            ( 'MetaCons "ZonedTimeG" 'PrefixI 'False)
            ( S1
                ( 'MetaSel
                    'Nothing
                    'NoSourceUnpackedness
                    'NoSourceStrictness
                    'DecidedLazy
                )
                (Rec0 (Param 0 Integer, (Int, Bool, String)))
            )
        )
  from (ZonedTimeG zt) =
    let TimeZone a b c = zonedTimeZone zt
     in M3K (StarParam (utcToInteger (zonedTimeToUTC zt)), (a, b, c))
  to (M3K (StarParam secs, (a, b, c))) = ZonedTimeG (utcToZonedTime (TimeZone a b c) (picoSecondsToUtc secs))

-- | wrapper for 'Day'
newtype DayG p = DayG Day
  deriving (Eq, Show, Ord, Read, Enum, FormatTime) via Day

instance ParseTime (DayG p) where
  parseTimeSpecifier _ = TI.parseTimeSpecifier (Proxy @Day)
  buildTime l xs = DayG <$> TI.buildTime l xs

-- | generic instance for 'DayG'
instance Generic (DayG p) where
  type
    Rep (DayG p) =
      D1
        ( 'MetaData "DayG" "Main" "main" 'True)
        ( C1
            ( 'MetaCons "DayG" 'PrefixI 'False)
            ( S1
                ( 'MetaSel
                    'Nothing
                    'NoSourceUnpackedness
                    'NoSourceStrictness
                    'DecidedLazy
                )
                (Rec0 (Param 0 Integer))
            )
        )
  from (DayG day) = M3K (StarParam (toModifiedJulianDay day))
  to (M3K (StarParam days)) = DayG (ModifiedJulianDay days)

-- | wrapper for 'NominalDiffTime'
newtype NominalDiffTimeG p = NominalDiffTimeG NominalDiffTime
  deriving (Eq, Show, Ord, Enum, Num, Real, RealFrac, Fractional, FormatTime) via NominalDiffTime

instance ParseTime (NominalDiffTimeG p) where
  parseTimeSpecifier _ = TI.parseTimeSpecifier (Proxy @NominalDiffTime)
  buildTime l xs = NominalDiffTimeG <$> TI.buildTime l xs

-- | generic instance for 'NominalDiffTimeG'
instance Generic (NominalDiffTimeG p) where
  type
    Rep (NominalDiffTimeG p) =
      D1
        ( 'MetaData "NominalDiffTimeG" "Main" "main" 'True)
        ( C1
            ( 'MetaCons "NominalDiffTimeG" 'PrefixI 'False)
            ( S1
                ( 'MetaSel
                    'Nothing
                    'NoSourceUnpackedness
                    'NoSourceStrictness
                    'DecidedLazy
                )
                (Rec0 (Param 0 RationalT))
            )
        )
  from (NominalDiffTimeG nomtime) = M3K (StarParam (splitRational $ toRational nomtime))
  to (M3K (StarParam secs)) = NominalDiffTimeG (fromRational $ joinRational secs)

-- | wrapper for 'DiffTime'
newtype DiffTimeG p = DiffTimeG DiffTime
  deriving (Eq, Show, Ord, Enum, Num, Real, RealFrac, Fractional, FormatTime) via DiffTime

instance ParseTime (DiffTimeG p) where
  parseTimeSpecifier _ = TI.parseTimeSpecifier (Proxy @DiffTime)
  buildTime l xs = DiffTimeG <$> TI.buildTime l xs

-- | generic instance for 'DiffTimeG'
instance Generic (DiffTimeG p) where
  type
    Rep (DiffTimeG p) =
      D1
        ( 'MetaData "DiffTimeG" "Main" "main" 'True)
        ( C1
            ( 'MetaCons "DiffTimeG" 'PrefixI 'False)
            ( S1
                ( 'MetaSel
                    'Nothing
                    'NoSourceUnpackedness
                    'NoSourceStrictness
                    'DecidedLazy
                )
                (Rec0 (Param 0 Integer))
            )
        )
  from (DiffTimeG difftime) = M3K (StarParam (diffTimeToPicoseconds difftime))
  to (M3K (StarParam secs)) = DiffTimeG (picosecondsToDiffTime secs)

-- | wrapper for 'TimeOfDay'
newtype TimeOfDayG p = TimeOfDayG TimeOfDay
  deriving (Eq, Show, Ord, Read, FormatTime) via TimeOfDay

instance ParseTime (TimeOfDayG p) where
  parseTimeSpecifier _ = TI.parseTimeSpecifier (Proxy @TimeOfDay)
  buildTime l xs = TimeOfDayG <$> TI.buildTime l xs

-- | generic instance for 'TimeOfDayG'
instance Generic (TimeOfDayG p) where
  type
    Rep (TimeOfDayG p) =
      D1
        ( 'MetaData "TimeOfDayG" "Main" "main" 'True)
        ( C1
            ( 'MetaCons "TimeOfDayG" 'PrefixI 'False)
            ( S1
                ( 'MetaSel
                    'Nothing
                    'NoSourceUnpackedness
                    'NoSourceStrictness
                    'DecidedLazy
                )
                (Rec0 (Param 0 RationalT))
            )
        )
  from (TimeOfDayG tod) = M3K (StarParam (splitRational $ timeOfDayToDayFraction tod))
  to (M3K (StarParam rat)) = TimeOfDayG (dayFractionToTimeOfDay $ joinRational rat)

-- | wrapper for 'Ratio'
newtype RatioG p a = RatioG (Ratio a)
  deriving (Eq, Show, Ord, Enum, Num, Real, RealFrac, Fractional) via Ratio a

-- | convenience wrapper
type RationalG p = RatioG p Integer

-- | generic instance for 'RatioG'
instance Integral a => Generic (RatioG p a) where
  type
    Rep (RatioG p a) =
      D1
        ( 'MetaData "RatioG" "Main" "main" 'True)
        ( C1
            ( 'MetaCons "RatioG" 'PrefixI 'False)
            ( S1
                ( 'MetaSel
                    'Nothing
                    'NoSourceUnpackedness
                    'NoSourceStrictness
                    'DecidedLazy
                )
                (Rec0 (a, a))
            )
        )
  from (RatioG r2) = M3K (numerator r2, denominator r2)
  to (M3K rat) = RatioG (uncurry (%) rat)

-- | pattern synonym for a complex generics construct
{-# COMPLETE M3K #-}

pattern M3K :: x -> M1 i1 c1 (M1 i2 c2 (M1 i3 c3 (K1 i4 x))) p
pattern M3K x = M1 (M1 (M1 (K1 x)))

-- | a pair of Integers that holds the internal representation of 'Rational'
type RationalT = (Integer, Integer)

-- | a pair of Integers that holds the internal representation of 'Ratio'
type RatioT a = (a, a)

-- | splits a rational into a tuple
splitRational :: Ratio a -> RatioT a
splitRational = numerator &&& denominator

-- | unsplits a tuple into a rational
joinRational :: Integral a => RatioT a -> Ratio a
joinRational = uncurry (%)

-- | converts 'LocalTime' into a integer tuple that for use with generics
localTimeToPair :: LocalTime -> (Integer, RationalT)
localTimeToPair (LocalTime day tod) = (toModifiedJulianDay day, splitRational $ timeOfDayToDayFraction tod)

-- | reconstruct 'LocalTime' from an integer tuple: see 'localTimeToPair'
pairToLocalTime :: (Integer, RationalT) -> LocalTime
pairToLocalTime (day, tod) = LocalTime (ModifiedJulianDay day) (dayFractionToTimeOfDay $ joinRational tod)

-- | convenience wrapper for using UTCTimeG in generic signatures
type UTCTimeG' = UTCTimeG ()

-- | convenience wrapper for using UTCTimeG in generic signatures
type LocalTimeG' = LocalTimeG ()

-- | convenience wrapper for using UTCTimeG in generic signatures
type ZonedTimeG' = ZonedTimeG ()

-- | convenience wrapper for using UTCTimeG in generic signatures
type DayG' = DayG ()

-- | convenience wrapper for using UTCTimeG in generic signatures
type NominalDiffTimeG' = NominalDiffTimeG ()

-- | convenience wrapper for using UTCTimeG in generic signatures
type DiffTimeG' = DiffTimeG ()

-- | convenience wrapper for using UTCTimeG in generic signatures
type TimeOfDayG' = TimeOfDayG ()

-- | convenience wrapper for using UTCTimeG in generic signatures
type RatioG' a = RatioG () a

-- | gets the relative constructor index for the value
type GetIndex' :: (Type -> Type) -> Constraint
class GetIndex' f where
  getIndex' :: f p -> Int
  size :: Int

instance GetIndex' f => GetIndex' (D1 t f) where
  getIndex' (M1 x) = getIndex' x
  size = size @f

-- https://stackoverflow.com/questions/51811617/get-constructor-index-using-generics-in-haskell
-- We've reached a constructor. It doesn't matter what it
-- looks like; the results will be the same regardless.
instance GetIndex' (C1 t f) where
  getIndex' _ = 0
  size = 1

instance GetIndex' V1 where
  getIndex' v = case v of {}
  size = 0

instance (GetIndex' f, GetIndex' g) => GetIndex' (f :+: g) where
  getIndex' (L1 x) = getIndex' x
  getIndex' (R1 x) = size @f + getIndex' x
  size = size @f + size @g

-- | get relative constructor index of the value
getIndex :: (Generic a, GetIndex' (Rep a)) => a -> Int
getIndex = getIndex' . from

-- | get relative constructor index of the value
getSize :: forall a. (GetIndex' (Rep a)) => Int
getSize = size @(Rep a)

-- | get the list of constructor names for a type
type ConNames :: (Type -> Type) -> Constraint
class ConNames f where
  gconNames :: f a -> NonEmpty String
  gconNameOf :: f a -> String

instance (ConNames f, ConNames g) => ConNames (f :+: g) where
  gconNames (_ :: (f :+: g) a) =
    gconNames (normalError "left sum" :: f a)
      <> gconNames (normalError "right sum" :: g a)
  gconNameOf = \case
    L1 x -> gconNameOf x
    R1 x -> gconNameOf x

instance (ConNames f) => ConNames (D1 c f) where
  gconNames (_ :: (D1 c f) a) = gconNames (normalError "D1" :: f a)
  gconNameOf (M1 x) = gconNameOf x

instance (Constructor c) => ConNames (C1 c f) where
  gconNames x = pure (conName x)
  gconNameOf x = conName x

-- | Return the name of all the constructors of the type of the given term.
conNames :: (Generic a, ConNames (Rep a)) => a -> NonEmpty String
conNames x = gconNames (normalError "root" `asTypeOf` from x)

-- | Return the name of the constructor of the given term
conNameOf :: (ConNames (Rep a), Generic a) => a -> String
conNameOf x = gconNameOf (from x)
