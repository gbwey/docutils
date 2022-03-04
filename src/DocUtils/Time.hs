{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : DocUtils.Time
Description : time conversion methods
Copyright   : (c) Grant Weyburne, 2022
License     : BSD-3
-}
module DocUtils.Time (
  HHMMSSDD (..),
  utcToInteger,
  picoSecondsToUtc,
  secondsToUtc,
  toUtcTime,
  toUtcTime',
  roundSecondsZT,
  getZonedTimeFloor,
  localUTC,
  formatUtc,
  getUtcTimeCorrected,
  timeSpecDuration,
  timeSpecSeconds,
  secondsDuration,
  to100Seconds,
  toSeconds,
  fmtZt,
  fmtZtDate,
  _YYYYMMDD_HHMMSS,
  _YYYYMMDD,
  _HHMMSS,
  _HHMM,
) where

import Control.Monad.IO.Class
import Data.Coerce
import Data.Fixed
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX
import GHC.Generics
import System.Clock
import System.Time.Extra (showDuration)

-- | convert 'UTCTime' to pico seconds
utcToInteger :: UTCTime -> Integer
utcToInteger = coerce . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds
{-# INLINEABLE utcToInteger #-}

-- ie 1_000_000_000_000 is one second ie 12 zeroes

-- | convert pico seconds to 'UTCTime'
picoSecondsToUtc :: Integer -> UTCTime
picoSecondsToUtc = posixSecondsToUTCTime . secondsToNominalDiffTime . coerce
{-# INLINEABLE picoSecondsToUtc #-}

-- | convert seconds to 'UTCTime'
secondsToUtc :: Integer -> UTCTime
secondsToUtc = posixSecondsToUTCTime . fromInteger
{-# INLINEABLE secondsToUtc #-}

-- | convert a valid date time to 'UTCTime'
toUtcTime :: (Int, Int, Int) -> (Int, Int, Int) -> Maybe UTCTime
toUtcTime (y, m, d) (hh, mm, ss) = flip UTCTime (timeOfDayToTime (TimeOfDay hh mm (fromIntegral ss))) <$> fromGregorianValid (fromIntegral y) m d
{-# INLINEABLE toUtcTime #-}

-- | convert date time to 'UTCTime'
toUtcTime' :: (Int, Int, Int) -> (Int, Int, Int) -> UTCTime
toUtcTime' (y, m, d) (hh, mm, ss) = flip UTCTime (timeOfDayToTime (TimeOfDay hh mm (fromIntegral ss))) $ fromGregorian (fromIntegral y) m d
{-# INLINEABLE toUtcTime' #-}

-- | round down the 'ZonedTime' to the nearest second
roundSecondsZT :: ZonedTime -> ZonedTime
roundSecondsZT zt =
  let loct = zonedTimeToLocalTime zt
      tod = localTimeOfDay (zonedTimeToLocalTime zt)
      tsec :: Fixed E12
      tsec = fromIntegral @Int (floor (todSec (localTimeOfDay (zonedTimeToLocalTime zt))))
   in zt{zonedTimeToLocalTime = loct{localTimeOfDay = tod{todSec = tsec}}}
{-# INLINEABLE roundSecondsZT #-}

-- | get current 'ZonedTime' rounded down to the nearest second
getZonedTimeFloor :: MonadIO m => m ZonedTime
getZonedTimeFloor = liftIO $ roundSecondsZT <$> getZonedTime
{-# INLINEABLE getZonedTimeFloor #-}

-- | convert 'ZonedTime' to 'UTCTime' using utc timezone
localUTC :: ZonedTime -> UTCTime
localUTC = localTimeToUTC utc . zonedTimeToLocalTime . roundSecondsZT
{-# INLINEABLE localUTC #-}

-- | format 'UTCTime'
formatUtc :: UTCTime -> String
formatUtc = formatTime defaultTimeLocale _YYYYMMDD_HHMMSS
{-# INLINEABLE formatUtc #-}

-- | get current 'UTCTime' rounded down to the nearest second
getUtcTimeCorrected :: MonadIO m => m UTCTime
getUtcTimeCorrected = localTimeToUTC utc . zonedTimeToLocalTime <$> liftIO getZonedTimeFloor
{-# INLINEABLE getUtcTimeCorrected #-}

-- | pretty show the duration
timeSpecDuration :: TimeSpec -> Text
timeSpecDuration = T.pack . showDuration . fromIntegral . sec
{-# INLINEABLE timeSpecDuration #-}

-- | extract the number of seconds from 'TimeSpec'
timeSpecSeconds :: TimeSpec -> Int
timeSpecSeconds = fromIntegral . sec
{-# INLINEABLE timeSpecSeconds #-}

-- | pretty show the duration in seconds
secondsDuration :: Int -> Text
secondsDuration = T.pack . showDuration . fromIntegral

-- | a record holding time plus hundreths of second
data HHMMSSDD = HHMMSSDD
  { hdHH :: !Int
  , hdMM :: !Int
  , hdSS :: !Int
  , hd100SS :: !Int
  }
  deriving stock (Generic, Ord, Show, Eq)

-- | convert 'HHMMSSDD' to centiseconds
to100Seconds :: HHMMSSDD -> Int
to100Seconds z = toSeconds z * 100 + hd100SS z
{-# INLINEABLE to100Seconds #-}

-- | convert 'HHMMSSDD' to seconds
toSeconds :: HHMMSSDD -> Int
toSeconds x = hdHH x * 3600 + hdMM x * 60 + hdSS x + div (hd100SS x) 100
{-# INLINEABLE toSeconds #-}

-- | format date component of 'ZonedTime'
fmtZt :: ZonedTime -> String
fmtZt = formatTime defaultTimeLocale "%T"
{-# INLINEABLE fmtZt #-}

-- | format 'ZonedTime'
fmtZtDate :: ZonedTime -> String
fmtZtDate = formatTime defaultTimeLocale "%FT%T"
{-# INLINEABLE fmtZtDate #-}

-- | date time format
_YYYYMMDD_HHMMSS :: (Semigroup s, IsString s) => s
_YYYYMMDD_HHMMSS = _YYYYMMDD <> "_" <> _HHMMSS
{-# INLINEABLE _YYYYMMDD_HHMMSS #-}

-- | various date and time formats
_YYYYMMDD, _HHMMSS, _HHMM :: IsString s => s
_YYYYMMDD = "%Y%m%d"
{-# INLINEABLE _YYYYMMDD #-}
_HHMMSS = "%H%M%S"
{-# INLINEABLE _HHMMSS #-}
_HHMM = "%H%M"
{-# INLINEABLE _HHMM #-}
