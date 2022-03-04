{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : DocUtils.Parser
Description : parsers date/time and ranges
Copyright   : (c) Grant Weyburne, 2022
License     : BSD-3
-}
module DocUtils.Parser (
  parseDurationP,
  rangeP,
  utcTimeP,
  validateRange,
  chkDay,
  parseTimeAll,
  parseIntRanges,
  parseIntRangeP,
  intP,
  floatZ,
  theseP,
) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Char
import qualified Data.List.Extra as LE
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as N
import Data.Maybe
import qualified Data.Text as T
import Data.These
import Data.These.Combinators
import Data.Time
import Data.Time.Format.ISO8601
import DocUtils.Time
import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as Z
import qualified Text.Megaparsec.Char as Z
import qualified Text.Megaparsec.Char.Lexer as ZL
import Text.Read (readMaybe)
import Utils.Error
import Utils.NonEmpty
import Utils.Parser
import Utils.Positive

-- | parse duration
parseDurationP :: ZParser HHMMSSDD
parseDurationP = do
  Z.notFollowedBy Z.eof <?> "some data!"
  hdHH <- Z.try (ZL.decimal <* "h" <?> "hours") <|> pure 0
  hdMM <- Z.try (ZL.decimal <* "m" <?> "minutes") <|> pure 0
  (hdSS, truncate . (* 100) -> hd100SS) <- (floatZ <* "s" <?> "seconds") <|> pure (0, 0)
  Z.eof <?> "parseDurationP: extra data"
  return HHMMSSDD{..}

-- | parse "n" digits and checks that value is between "s" and "e"
rangeP :: (Show a, Ord a, Read a) => String -> Positive -> (a, a) -> ZParser a
rangeP msg' n (s, e)
  | s > e = zfail $ msg "start is greater than end! " <> show (s, e)
  | otherwise = do
      x <- Z.count (unPositive n) Z.digitChar <?> msg "invalid digits"
      case readMaybe x of
        Nothing -> zfail $ msg "failed to read x=[" <> x <> "]"
        Just i
          | i < s || i > e -> zfail $ msg (show i) <> " not in range [" <> show s <> ".." <> show e <> "]"
          | otherwise -> return i
 where
  msg :: String -> String
  msg = (("rangeP:" <> msg' <> ":") <>)

-- todo: this is crap but we need to pass msg around to get the context of the errors!

-- | parse 'UTCTime' in the format yyyymmdd_hhmmss
utcTimeP :: String -> ZParser UTCTime
utcTimeP msg' = do
  y <- rangeP (msg "year") _4P (1900, 2100)
  m <- rangeP (msg "month") _2P (1, 12)
  d <- rangeP (msg "day") _2P (1, 31)
  void "_" <?> msg "underscore between date and time"
  hh <- rangeP (msg "hour") _2P (0, 23)
  mm <- rangeP (msg "minute") _2P (0, 59)
  ss <- rangeP (msg "second") _2P (0, 59)
  case toUtcTime (y, m, d) (hh, mm, ss) of
    Just ut -> return ut
    Nothing -> zfail $ msg "invalid date " <> show (y, m, d)
 where
  msg :: String -> String
  msg = (("utcTimeP:" <> msg' <> ":") <>)

-- | validate the string "s" is in range
validateRange ::
  (Read a, Ord a, Show a) =>
  String ->
  These a a ->
  String ->
  Either String a
validateRange msg th s = do
  infomsg <-
    case th of
      These f t
        | f > t -> Left $ msg ++ " from>to!! ie " ++ show f ++ " > " ++ show t
        | otherwise -> pure $ "[" <> show f <> " .. " <> show t <> "]"
      This f -> pure $ "[" <> show f <> " .. ]"
      That t -> pure $ "[ .. " <> show t <> "]"
  left (\x -> msg <> ": " <> x <> ": valid range is " <> infomsg) $ do
    a <- maybe (Left $ "invalid number[" ++ s ++ "]") Right $ readMaybe s
    case justHere th of
      Nothing -> pure ()
      Just f
        | a >= f -> pure ()
        | otherwise -> Left $ show a ++ " < " ++ show f
    case justThere th of
      Nothing -> pure ()
      Just t
        | a <= t -> pure ()
        | otherwise -> Left $ show a ++ " > " ++ show t
    return a

-- | parse a day from a string in "YYYYMMDD" format
chkDay :: String -> Either String Day
chkDay s' = lmsg msg $ do
  -- remove punctuation first
  ns <- fromList1LR $ filter (`notElem` ("_-:/ .\\" :: String)) s'
  unless (all isDigit ns) $ Left $ "invalid digit(s) [" ++ N.filter (not . isDigit) ns ++ "]"
  s1 <- lengthExact1 _8P ns
  case parseTimeM True defaultTimeLocale _YYYYMMDD (N.toList s1) of
    Just dy -> return dy
    Nothing -> Left $ "invalid day[" ++ N.toList s1 ++ "] original value[" ++ s' ++ "]"
 where
  msg :: String
  msg = " :format is YYYYYMMDD (8 digits)"

-- | parse a datetime in iso8601 format
parseTimeAll :: forall t. (ISO8601 t, ParseTime t) => String -> [t]
parseTimeAll s =
  let fmts :: [String]
      fmts = [a <> b <> c | a <- ["%F %T", "%F", "%FT%T"], b <- ["", "%Q"], c <- ["", " %Z"]]
   in catMaybes $ iso8601ParseM s : map (\fmt -> parseTimeM True defaultTimeLocale fmt s) fmts

-- | parse a set of ranges using 'parseIntRangeP'
parseIntRanges :: String -> Either String (NonEmpty (NonEmpty Int))
parseIntRanges s = left T.unpack $ do
  let xs = LE.wordsBy (\c -> not (isDigit c || c == '.' || c == '-')) s
  yss <- mapM (runP parseIntRangeP . T.pack) xs
  case yss of
    [] -> Left "parseIntRanges: no matches found"
    n : ns -> return (n :| ns)

-- | parse a range: either a singleton or range of values separated by ".." or "-"
parseIntRangeP :: ZParser (NonEmpty Int)
parseIntRangeP =
  Z.try
    ( do
        i <- ZL.decimal <?> msg "lhs of ../_"
        void (".." <|> "-") <?> msg "dotdot or underscore"
        j <- ZL.decimal <?> msg "rhs of ../_"
        Z.eof <?> msg "extra stuff after lhs..rhs"
        case [i .. j] of
          [] -> zfail $ msg "start index > end index ie " <> show (i, j)
          a : as -> return (a :| as)
    )
    <|> ( do
            i <- ZL.decimal <?> msg "single number"
            Z.eof <?> msg "extra stuff after single number"
            return $ pure i
        )
 where
  msg :: String -> String
  msg = ("parseIntRangeP:" <>)

-- | parses "n" digits
{-# INLINE intP #-}
intP :: Read a => Int -> ZParser a
intP n = do
  x <- Z.count n Z.digitChar <?> "invalid digits"
  case readMaybe x of
    Nothing -> zfail $ "intP: failed to read x=[" <> x <> "]"
    Just i -> return i

-- | parser for a 'Float'
{-# INLINE floatZ #-}
floatZ :: ZParser (Int, Double)
floatZ = do
  ret <- (<>) <$> some Z.digitChar <*> ((:) <$> Z.char '.' <*> some Z.digitChar <|> mempty)
  return $ properFraction $ fromMaybe (programmerError "floatZ: read failed") $ readMaybe ret

-- | 'These' for parsers so we can trace where the parse originated
{-# INLINE theseP #-}
theseP :: Alternative f => f a -> f b -> f (These a b)
theseP p q =
  ((\x y -> maybe (This x) (These x) y) <$> p <*> optional q)
    <|> ((\y x -> maybe (That y) (`These` y) x) <$> q <*> optional p)
