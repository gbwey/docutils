{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : DocUtils.Divvy
Description : detailed comparison of two dictionaries
Copyright   : (c) Grant Weyburne, 2022
License     : BSD-3

returns those in "A" and not "B" and vice versa
as well as those common to both with a comparison of those values
it requires that each input dictionary has unique keys
-}
module DocUtils.Divvy (
  divvyKeyed,
  --  mapEachPiece,
  --  divvyKeyedInternal,
  argIso,
  --  unArg,
  --  reArg,
) where

import Control.Arrow
import Control.Lens
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Merge.Strict as MS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
import Data.These
import GHC.Stack
import Utils.Error
import Utils.NonEmpty

-- | compares 2 lists based on a comparator but unwraps 'Arg'
divvyKeyed ::
  forall w x a b.
  (HasCallStack, Ord w, Ord x, Ord a, Ord b) =>
  (a -> x) ->
  (b -> x) ->
  [(a, w)] ->
  [(b, w)] ->
  (Set (a, w), Set (b, w), Map Ordering (NonEmpty ((a, w), (b, w))))
divvyKeyed ax bx tp1 tp2 =
  let !_ = chk tp1
      !_ = chk tp2
      (a, b, c) = divvyKeyedInternal ax bx (Set.fromList (map reArg tp1)) (Set.fromList (map reArg tp2))
   in (Set.map unArg a, Set.map unArg b, M.map (fmap (unArg *** unArg)) c)
 where
  chk :: forall c. Ord c => [(c, w)] -> ()
  chk xs = case fst $ findDupsBy fst xs of
    [] -> ()
    o@(_ : _) -> normalError $ "duplicates in divvyKeyed " ++ show ((fmap . fmap) fst o)

-- | internal method for merging two maps
{-# INLINE mapEachPiece #-}
mapEachPiece ::
  Ord k =>
  (k -> a -> c) ->
  (k -> b -> c) ->
  (k -> a -> b -> c) ->
  Map k a ->
  Map k b ->
  Map k c
mapEachPiece f g h = MS.merge (MS.mapMissing f) (MS.mapMissing g) (MS.zipWithMatched h)

-- most useful so far
-- using Set means no duplicates
-- uses Arg so dont need unnecessary Ord a, Ord b
-- M.fromSet keeps in Set as opposed to bouncing out and back to Map
-- all Map are intrinisically NonEmpty for their elements as opposed to a list

-- | compares 2 lists based on a comparator
divvyKeyedInternal ::
  forall k w a b.
  (HasCallStack, Ord k, Ord w) =>
  (a -> k) ->
  (b -> k) ->
  Set (Arg w a) ->
  Set (Arg w b) ->
  (Set (Arg w a), Set (Arg w b), Map Ordering (NonEmpty (Arg w a, Arg w b)))
divvyKeyedInternal ax bx tp1 tp2 =
  let m1, m2 :: Map k (These (Arg w a) (Arg w b))
      ff ::
        forall t.
        (t -> k) ->
        Map (Arg w t) (These (Arg w a) (Arg w b)) ->
        Map k (These (Arg w a) (Arg w b))
      ff f m = M.mapKeysWith (normalError "divvyKeyedInternal: bad key function: creates duplicates") (\(Arg _ a) -> f a) m -- stays in Set
      m1 = ff ax $ M.fromSet This tp1
      m2 = ff bx $ M.fromSet That tp2
      mx =
        mapEachPiece
          (const id)
          (const id)
          ( const
              ( \th th1 -> case (th, th1) of
                  (This a, That b) -> These a b
                  (That b, This a) -> These a b
                  _other -> programmerError "cant happen as we have passed in Sets so no duplicates"
              )
          )
          m1
          m2
      (as, bs, zs) = partitionThese $ M.elems mx
   in (Set.fromList as, Set.fromList bs, M.fromListWith (<>) (map g zs))
 where
  g :: forall f. Applicative f => (Arg w a, Arg w b) -> (Ordering, f (Arg w a, Arg w b))
  g z@(Arg a _, Arg a' _) = (compare a a', pure z)

-- | iso from 'Arg' to a tuple
argIso :: Iso (Arg a b) (Arg a' b') (b, a) (b', a')
argIso = iso unArg reArg

-- | unwrap 'Arg' into a tuple
{-# INLINE unArg #-}
unArg :: Arg a b -> (b, a)
unArg (Arg a b) = (b, a)

-- | wrap a tuple into 'Arg'
{-# INLINE reArg #-}
reArg :: (a, b) -> Arg b a
reArg (a, b) = Arg b a
