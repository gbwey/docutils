{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{- |
Module      : DocUtils.Condition
Description : validation methods using 'Validation'
Copyright   : (c) Grant Weyburne, 2022
License     : BSD-3
-}
module DocUtils.Condition (
  VE,
  ConditionType (..),
  Condition (..),
  ConditionErrors' (..),
  pattern ConditionErrors,
  prtV,
  expectVFailureWith,
  forceError,
  forceErrorIO,
  expectNoDups,
  expectOneM,
  expectOne,
  expectNonEmpty,
  expectUniqueM,
  expectUnique,
  expectLeft,
  expectRight,
  expectTrue,
  expectEq,
  expectFail,
  expectEmpty,
  expectAscending,
  expectAscending',
  isSequence,
  isSequence',
  expectLeftWithT,
  expectLeftWith,
  expectNoDupsBy,
  expectTheseOne,
  toLeftF,
  toRightF,
  toOneF,
  toMaybeF,
  fromVE,
  fromVET,
  expectAllEqual,
  expectAllEqualBy,
  expectLengthAllEqual1,
  expectLengthAllEqual0,
) where

import Control.Applicative
import Control.Arrow
import qualified Control.Exception as E
import Data.Containers.ListUtils (nubOrd)
import Data.Either
import Data.Foldable
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as N
import Data.Semigroup.Foldable
import Data.Text (Text)
import qualified Data.Text as T
import Data.These
import DocUtils.Doc
import GHC.Generics (Generic)
import GHC.Stack
import Utils.Error
import Utils.List
import Utils.NonEmpty
import Utils.Positive
import Utils.ZipNonEmpty
import Validation

-- | type synonym holding the result of the validation
type VE a = Validation (NonEmpty (ConditionType, Text)) a

-- | pretty print the validation results
prtV :: (Show e, Show a) => Validation e a -> IO ()
prtV = putStrLn . validation psiS psiS

-- | validate that the expected errors match the actual failure and that the error messages match the infix strings
expectVFailureWith ::
  Show a =>
  [Text] ->
  NonEmpty ConditionType ->
  VE a ->
  Either String ()
expectVFailureWith _ _ (Success a) = Left $ "expected failure but got success " ++ psiS a
expectVFailureWith ns cts (Failure tps) =
  let (cts', ns') = N.unzip tps
   in if cts /= cts'
        then Left $ "failed to match on conditiontypes actual=" ++ psiS cts' ++ " expected=" ++ psiS cts
        else
          if all (\t -> any (t `T.isInfixOf`) ns') ns
            then Right ()
            else Left $ "infix strings did not match: actual" ++ psiS (N.toList ns') ++ "] infix=" ++ psiS ns

-- | document the type of failure
data Condition = Pre | Invariant | Post deriving stock (Show, Eq)

-- | ADT with the possible errors
data ConditionType
  = ExpectUniqueM
  | ExpectUnique
  | ExpectOneM
  | ExpectOne
  | ExpectNonEmpty
  | ExpectNoDups
  | ExpectAscending
  | ExpectEq
  | ExpectEmpty
  | ExpectFail
  | ExpectLeft
  | ExpectRight
  | ExpectTheseOne
  | ExpectAllEqual
  | ExpectAllEqualLength
  deriving stock (Show, Eq, Generic, Enum, Bounded)

-- | holds a non empty list of validation errors
data ConditionErrors' = ConditionErrors'
  { cesErrors :: !(NonEmpty (ConditionType, Text))
  , cesCallStack :: !Text
  }
  deriving stock (Generic, Eq)

instance E.Exception ConditionErrors'

-- | pattern synonym for wrapping the 'ConditionErrors'' in a stack trace
pattern ConditionErrors ::
  () =>
  HasCallStack =>
  NonEmpty (ConditionType, Text) ->
  ConditionErrors'
pattern ConditionErrors ns <-
  ConditionErrors' ns _
  where
    ConditionErrors ns = ConditionErrors' ns (T.pack (prettyCallStack callStack))

instance Show ConditionErrors' where
  show (ConditionErrors' ns c) = psiS ns <> "\n" <> T.unpack c

-- | force an error if the validation failed
forceError :: HasCallStack => VE a -> a
forceError = validation (E.throw . ConditionErrors) id

-- | force an IO error if the validation failed
forceErrorIO :: VE a -> IO a
forceErrorIO = validation (E.throwIO . ConditionErrors) return

-- | validate that the list contains no duplicates
expectNoDups :: (Show a, Ord a) => Condition -> Text -> [a] -> VE [a]
expectNoDups pp errmsg = expectNoDupsBy pp errmsg id

-- | validate that the list contains zero or one values
expectOneM :: Show a => Condition -> Text -> [a] -> VE (Maybe a)
expectOneM pp errmsg =
  \case
    [] -> pure Nothing
    [a] -> pure (Just a)
    z@(_ : _ : _) ->
      failure $
        (ExpectOneM,) $
          T.pack (show pp)
            <> " expected zero/one items in list: "
            <> errmsg
            <> " length="
            <> packLen z
            <> " values\n"
            <> psiT z

-- | validate that the list contains exactly one value
expectOne :: Show a => Condition -> Text -> [a] -> VE a
expectOne pp errmsg =
  \case
    [] ->
      failure $
        (ExpectOne,) $
          T.pack (show pp)
            <> " expected one item in list but found none: "
            <> errmsg
    [a] -> pure a
    z@(_ : _ : _) ->
      failure $
        (ExpectOne,) $
          T.pack (show pp)
            <> " expected one item in list: "
            <> errmsg
            <> " length="
            <> packLen z
            <> " values\n"
            <> psiT z

-- | validate that the list contains at least one element
expectNonEmpty :: Condition -> Text -> [a] -> VE (NonEmpty a)
expectNonEmpty pp errmsg =
  \case
    [] ->
      failure $
        (ExpectNonEmpty,) $
          T.pack (show pp)
            <> " expected at least one item in list but found none: "
            <> errmsg
    a : as -> pure (a :| as)

-- | validate that the list contains zero or one unique value
expectUniqueM :: (Show a, Ord a) => Condition -> Text -> [a] -> VE (Maybe a)
expectUniqueM pp errmsg xs =
  case nubOrd xs of
    [] -> pure Nothing
    [a] -> pure (Just a)
    z@(_ : _ : _) ->
      failure $
        (ExpectUniqueM,) $
          T.pack (show pp)
            <> " expected zero/one unique item in list: "
            <> errmsg
            <> " length="
            <> packLen z
            <> " unique values\n"
            <> psiT z

-- | validate that the list contains exactly one unique value
expectUnique :: (Show a, Ord a) => Condition -> Text -> [a] -> VE a
expectUnique pp errmsg xs =
  case nubOrd xs of
    [] ->
      failure $
        (ExpectUnique,) $
          T.pack (show pp)
            <> " expected one unique item in list but found none: "
            <> errmsg
    [a] -> pure a
    z@(_ : _ : _) ->
      failure $
        (ExpectUnique,) $
          T.pack (show pp)
            <> " expected one unique item in list: "
            <> errmsg
            <> " length="
            <> packLen z
            <> " unique values\n"
            <> psiT z

-- | validate the value is a 'Left'
expectLeft :: Show x => Condition -> Text -> Either a x -> VE a
expectLeft pp errmsg =
  \case
    Right e ->
      failure $
        (ExpectLeft,) $
          T.pack (show pp)
            <> " expected Left but found Right "
            <> psiT e
            <> errmsg
    Left a -> pure a

-- | validate the value is a 'Right'
expectRight :: Show x => Condition -> Text -> Either x a -> VE a
expectRight pp errmsg =
  \case
    Left e ->
      failure $
        (ExpectRight,) $
          T.pack (show pp)
            <> " expected Right but found Left "
            <> psiT e
            <> errmsg
    Right a -> pure a

-- | validate the value is 'True'
expectTrue :: Condition -> Text -> Bool -> VE ()
expectTrue pp errmsg = expectEq pp errmsg True

-- | validate that these two values are the same
expectEq :: (Eq a, Show a) => Condition -> Text -> a -> a -> VE ()
expectEq pp errmsg a a'
  | a == a' = pure ()
  | otherwise =
      failure $
        (ExpectEq,) $
          T.pack (show pp)
            <> " expected a==a': "
            <> errmsg
            <> " a=\n"
            <> psiT a
            <> " a'=\n"
            <> psiT a'

-- | force a failure
expectFail :: Condition -> Text -> VE a
expectFail pp errmsg =
  failure $
    (ExpectFail,) $
      T.pack (show pp)
        <> " failed: "
        <> errmsg

-- | validate that the list is empty
expectEmpty :: Show a => Condition -> Text -> [a] -> VE ()
expectEmpty pp errmsg =
  \case
    [] -> pure ()
    z@(_ : _) ->
      failure $
        (ExpectEmpty,) $
          T.pack (show pp)
            <> " expected null: "
            <> errmsg
            <> " length="
            <> packLen z
            <> "\n"
            <> psiT z

-- | validate that the list is ascending starting at "i"
expectAscending :: (Integral a, Show a) => Int -> Condition -> Text -> [a] -> VE [a]
expectAscending i pp errmsg z =
  case isSequence (fromIntegral i) z of
    Just ret -> pure ret
    Nothing ->
      failure $
        (ExpectAscending,) $
          T.pack (show pp)
            <> " expected ascending sequence starting at "
            <> T.pack (show i)
            <> ":"
            <> errmsg
            <> "\n"
            <> psiCT z

-- | validate that the list is ascending
expectAscending' :: (Integral a, Show a) => Condition -> Text -> [a] -> VE [a]
expectAscending' pp errmsg z =
  case isSequence' z of
    Just ret -> pure ret
    Nothing ->
      failure $
        (ExpectAscending,) $
          T.pack (show pp)
            <> " expected ascending sequence:"
            <> errmsg
            <> "\n"
            <> psiCT z

-- | validate that the container has elements in ascending sequence starting at "i"
isSequence :: (Eq a, Enum a, Foldable t) => a -> t a -> Maybe (t a)
isSequence i ta
  | and $ zipWith (==) [i ..] (toList ta) = Just ta
  | otherwise = Nothing

-- | validate that the container has elements in ascending sequence
isSequence' :: (Eq a, Enum a, Foldable t) => t a -> Maybe (t a)
isSequence' ta =
  case toList ta of
    [] -> Just ta
    i : _ -> isSequence i ta

-- | expect a 'Left' with the text matching infix with "ns" but using 'Text' instead of String
expectLeftWithT :: Show a => NonEmpty String -> Either Text a -> Either String ()
expectLeftWithT xs = expectLeftWith xs . left T.unpack

-- | expect a 'Left' with the text matching infix with "ns"
expectLeftWith :: Show a => NonEmpty String -> Either String a -> Either String ()
expectLeftWith _ (Right a) = Left $ "expected fail but was actually successful " ++ show a
expectLeftWith ns (Left s)
  | all (`L.isInfixOf` s) ns = Right ()
  | otherwise = Left $ "found fail but infix string did not match: actual[" ++ s ++ "] infix[" ++ psiS ns ++ "]"

-- | looks for duplicates by "f" and if none found returns the original values
expectNoDupsBy :: (Show a, Show c, Ord c) => Condition -> Text -> (a -> c) -> [a] -> VE [a]
expectNoDupsBy pp errmsg f as =
  case fst $ findDupsBy f as of
    [] -> pure as
    o@(_ : _) ->
      failure $
        (ExpectNoDups,) $
          T.pack (show pp)
            <> " expected no duplicate values by function in the list: "
            <> errmsg
            <> " length="
            <> packLen o
            <> "\n"
            <> " "
            <> psiT ((fmap . fmap) (\(i, a) -> (i, a, f a)) o)

-- | only allow a combination of Left/Right but not multiple
expectTheseOne ::
  (Show a, Show b) =>
  Condition ->
  Text ->
  NonEmpty (Either a b) ->
  VE (These a b)
expectTheseOne pp errmsg lrs =
  case partitionEithersNE lrs of
    This (a :| as) -> case as of
      [] -> pure $ This a
      _ : _ ->
        failure $
          (ExpectTheseOne,) $
            T.pack (show pp)
              <> " expected one This value but found "
              <> errmsg
              <> " length="
              <> packLen (a : as)
              <> "\n"
              <> psiT (a : as)
    That (b :| bs) -> case bs of
      [] -> pure $ That b
      _ : _ ->
        failure $
          (ExpectTheseOne,) $
            T.pack (show pp)
              <> " expected one That value but found "
              <> errmsg
              <> " length="
              <> packLen (b : bs)
              <> "\n"
              <> psiT (b : bs)
    w@(These (a :| as) (b :| bs)) -> case (as, bs) of
      ([], []) -> pure $ These a b
      (_, _) ->
        failure $
          (ExpectTheseOne,) $
            T.pack (show pp)
              <> " expected one These value but found "
              <> errmsg
              <> " length="
              <> packLen (a : as)
              <> " and "
              <> packLen (b : bs)
              <> "\nlhs"
              <> psiT w

-- | force an error if not 'Left'
toLeftF :: (HasCallStack, Show x) => Text -> Either a x -> a
toLeftF msg = forceError . expectLeft Pre msg
{-# INLINEABLE toLeftF #-}

-- | force an error if not 'Right'
toRightF :: (HasCallStack, Show x) => Text -> Either x a -> a
toRightF msg = forceError . expectRight Pre msg
{-# INLINEABLE toRightF #-}

-- | force an error if not a single value
toOneF :: (HasCallStack, Show a) => Text -> [a] -> a
toOneF msg = forceError . expectOne Pre msg
{-# INLINEABLE toOneF #-}

-- | force an error if more than one value
toMaybeF :: (HasCallStack, Show a) => Text -> [a] -> Maybe a
toMaybeF msg = forceError . expectOneM Pre msg
{-# INLINEABLE toMaybeF #-}

-- | convert a 'Validation' result to an 'Either'
fromVE :: VE a -> Either String a
fromVE = left psiS . validationToEither
{-# INLINEABLE fromVE #-}

-- | convert a 'Validation' result to an 'Either'
fromVET :: VE a -> Either Text a
fromVET = left psiT . validationToEither
{-# INLINEABLE fromVET #-}

-- | validate that all the elements in a list are equal
expectAllEqual :: (Eq a, Show a) => Condition -> Text -> [a] -> VE ()
expectAllEqual pp errmsg = expectAllEqualBy pp errmsg (==)

-- | validate that all the elements in a list are equal by function "f"
expectAllEqualBy :: Show a => Condition -> Text -> (a -> a -> Bool) -> [a] -> VE ()
expectAllEqualBy pp errmsg f = go
 where
  go =
    \case
      [] -> pure ()
      [_] -> pure ()
      x : x' : xs
        | f x x' -> go (x' : xs)
        | otherwise ->
            failure $
              (ExpectAllEqual,) $
                T.pack (show pp)
                  <> " expected all values to be equal but found "
                  <> errmsg
                  <> " a="
                  <> psiT x
                  <> "\n"
                  <> " b="
                  <> psiT x'

-- gets the minimum size of all the lists using ZipNonEmpty: if at least one is finite then it will work or if only one value
-- if all are infinite and more than one entry then will hang

-- | checks that all the non empty containers are of the same size and returns that size
expectLengthAllEqual1 :: Foldable1 t => Condition -> Text -> NonEmpty (t a) -> VE Positive
expectLengthAllEqual1 pp errmsg ns = do
  let minlen = lengthPositive $ unZipNonEmpty $ N.head $ sequenceA $ traverse (ZipNonEmpty . toNonEmpty) ns
  case partitionEithers $ N.toList $ N.zipWith (\i x -> lmsg ("index=" ++ show i) $ lengthExact1 minlen (toNonEmpty x)) (0 :| [1 :: Int ..]) ns of
    ([], _) -> pure minlen
    (es@(_ : _), _) ->
      failure $
        (ExpectAllEqualLength,) $
          T.pack (show pp)
            <> " expected all values to be equal but found "
            <> "\n"
            <> psiT es
            <> " minlen="
            <> T.pack (show (unPositive minlen))
            <> " "
            <> errmsg

-- | checks that all the containers are of the same size and returns that size
expectLengthAllEqual0 :: Foldable t => Condition -> Text -> [t a] -> VE Int
expectLengthAllEqual0 pp errmsg =
  \case
    [] -> pure 0
    y : ys -> do
      let ns = y :| ys
      let minlen = length $ getZipList $ N.head $ sequenceA $ traverse (ZipList . toList) ns
      case partitionEithers $ N.toList $ N.zipWith (\i x -> lmsg ("index=" ++ show i) $ lengthExact minlen (toList x)) (0 :| [1 :: Int ..]) ns of
        ([], _) -> pure minlen
        (es@(_ : _), _) ->
          failure $
            (ExpectAllEqualLength,) $
              T.pack (show pp)
                <> " expected all values to be equal but found "
                <> "\n"
                <> psiT es
                <> " minlen="
                <> T.pack (show minlen)
                <> " "
                <> errmsg
