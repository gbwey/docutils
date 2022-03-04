{- |
Module      : DocUtils.Witherable
Description : extra methods for 'W.Witherable'
Copyright   : (c) Grant Weyburne, 2022
License     : BSD-3
-}
module DocUtils.Witherable (
  mapEitherF,
  mapTheseF,
  mapEitherA,
  mapTheseA,
  partitionEithersF,
  partitionThesesF,
  mapMaybeA,
) where

import Control.Applicative
import Data.These
import qualified Witherable as W

-- generalises filtering/partitionEither to [a]/NonEmpty a/ZipList/Map/IntMap but what else that is vaguely interesting?
-- Filterable doesnt work for fixed lists of course also not for NonEmpty cos could be empty
-- Witherable is good cos at least generalises traversable

-- | generalise 'W.mapMaybe' to work with 'Either'
mapEitherF :: W.Filterable f => (a -> Either b c) -> f a -> (f b, f c)
mapEitherF f =
  (,) <$> W.mapMaybe (either Just (pure Nothing) . f)
    <*> W.mapMaybe (either (pure Nothing) Just . f)

-- | generalise 'W.mapMaybe' to work with 'These'
mapTheseF :: W.Filterable f => (a -> These b c) -> f a -> (f b, f c)
mapTheseF f =
  (,) <$> W.mapMaybe (these Just (const Nothing) (const . Just) . f)
    <*> W.mapMaybe (these (const Nothing) Just (const Just) . f)

-- | Traverse the container with the given function, collecting the 'Left's and the 'Right's separately.
mapEitherA :: (W.Filterable f, Traversable f, Applicative p) => (a -> p (Either b c)) -> f a -> p (f b, f c)
mapEitherA f =
  liftA2 (,) <$> mapMaybeA (fmap (Just `either` pure Nothing) . f)
    <*> mapMaybeA (fmap (pure Nothing `either` Just) . f)

-- | similar to 'mapEitherA' but for 'These'
mapTheseA :: (W.Filterable f, Traversable f, Applicative p) => (a -> p (These b c)) -> f a -> p (f b, f c)
mapTheseA f =
  liftA2 (,) <$> mapMaybeA (fmap (these Just (const Nothing) (const . Just)) . f)
    <*> mapMaybeA (fmap (these (const Nothing) Just (const Just)) . f)

-- | @'Data.Either.partitionEithers' = 'mapEitherF' 'id'@
partitionEithersF :: W.Filterable f => f (Either a b) -> (f a, f b)
partitionEithersF = mapEitherF id

-- | @'Data.These.partitionThese' = 'mapTheseF' 'id'@
partitionThesesF :: W.Filterable f => f (These a b) -> (f a, f b)
partitionThesesF = mapTheseF id

-- | Traverse the container with the given function, dropping the elements for which it returns 'Nothing'.
mapMaybeA :: (W.Filterable f, Traversable f, Applicative p) => (a -> p (Maybe b)) -> f a -> p (f b)
mapMaybeA f xs = W.catMaybes <$> traverse f xs
