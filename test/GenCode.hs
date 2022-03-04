{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module GenCode where

import Data.Bool
import Data.Foldable
import qualified Data.List as L
import Data.Semigroup.Foldable
import Data.Text (Text)
import qualified Data.Text as T
import DocUtils.Doc
import Text.Shakespeare.Text
import Utils.Fold
import Utils.NonEmpty
import Utils.Positive

-- mapM_ (putStrLn . genListTupleC) (positiveRange' 2 20) -- to generate from two onwards
genListTupleC :: Positive -> String
genListTupleC (unPositive -> n)
  | n == 1 = error "genListTupleC: special case using One: you're own your own"
  | otherwise =
      let s1 = L.foldr (\_ z -> wrapParens (" 'SS " ++ z)) "'SZ" [1 .. n - 1]
          s2 =
            pFoldR
              ( \_ post z i -> case post of
                  [] -> "NE a" ++ show i ++ z
                  _ : _ -> "a" ++ show i ++ " :. " ++ z
              )
              mempty
              [1 .. n]
          s3 = L.intercalate ", " $ map (('a' :) . show) [1 .. n]
          s4 = wrapParens (replicate (n - 1) ',') <> " <$> " <> L.intercalate " <*> " (map (("afa " <>) . ('a' :) . show) [1 .. n])
       in T.unpack
            [st|instance ListTupleC #{s1} where
  toTupleC (#{s2}) = (#{s3})
  fromTupleC (#{s3}) = #{s2}
  traversalTupleC afa (#{s3}) = #{s4}|]

-- mapM_ (putStrLn . genTupleZ) (positiveRange' 2 20) -- to generate from two onwards
genTupleZ :: Positive -> String
genTupleZ (unPositive -> n)
  | n == 1 = error "genTupleZ: special case using One: you're own your own"
  | otherwise =
      let s1 = "N" ++ show n
          s2 =
            pFoldR
              ( \_ post z i -> case post of
                  [] -> "NE a" ++ show i ++ z
                  _ : _ -> "a" ++ show i ++ " :. " ++ z
              )
              mempty
              [1 .. n]
          s3 = L.intercalate ", " $ map (('a' :) . show) [1 .. n]
          s4 = L.intercalate ", " $ map (("a ~ " ++) . ('a' :) . show) [1 .. n]
       in T.unpack
            [st|instance (#{s4}) => TupleZ (#{s3}) #{s1} a where
  toTupleZ (#{s2}) = (#{s3})
  fromTupleZ (#{s3}) = #{s2}|]

{-
instance (a ~ a1, a ~ a2, a ~ a3, a ~ a4, a ~ a5, a ~ a6, a ~ a7, a ~ a8, a ~ a9, a ~ a10) => TupleZ (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) N10 a where
  toTupleZ (a1 :. a2 :. a3 :. a4 :. a5 :. a6 :. a7 :. a8 :. a9 :. NE a10) = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
  fromTupleZ (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = a1 :. a2 :. a3 :. a4 :. a5 :. a6 :. a7 :. a8 :. a9 :. NE a10
-}

--  traversalTupleC afa (a1,a2,a3) = (,,) <$> afa a1 <*> afb a2 <*> afb a3

-- for Peano.hs
genN :: Positive -> String
genN (unPositive -> n) = T.unpack [st|type N#{n} = |] <> pFoldL (\_ post z _ -> bool wrapParens id (null post) (" 'SS " ++ z)) "'SZ" [1 .. n - 1]

-- for Fin.hs
genFin :: Positive -> String
genFin (unPositive -> n) =
  let a = T.unpack [st|_F#{n} :: Fin |] <> wrapParens (pFoldL (\_ post z _ -> bool wrapParens id (null post) (" 'SS " ++ z)) "n" [1 .. n - 1])
      b = T.unpack [st|_F#{n} = |] <> pFoldL (\_ post z _ -> bool wrapParens id (null post) ("FS " ++ z)) "FZ" [1 .. n - 1]
   in L.unlines [a, b]

-- mapM_ (putStrLn . genListTupleT) (positiveRange' 2 20) -- to generate from two onwards
-- for ListF.hs
genListTupleT :: Positive -> String
genListTupleT p
  | p == _1P = error "genListTupleT: special case using One"
  | otherwise =
      let s2 = intercalate1 "," $ replicate1 @String p "a"
       in T.unpack [st|  ListTupleT N#{unPositive p} a = (#{s2})|]

genMatrixListC :: Int -> T.Text
genMatrixListC n =
  [st|instance (#{cs}) => MatrixListC '[#{vs}] where
  matrixListC (MS lst0) = #{rhs1}
  unMatrixListC (#{_MZMS} lst0) = #{rhs2}|]
 where
  _MZMS = bool @String "MS" "MZ" (n == 0)
  cs = L.intercalate ", " $ map (\i -> "ListFC w" <> show i) [1 .. n]
  vs = L.intercalate ", " $ map (\i -> "w" <> show i) [1 .. n]
  rhs1 = snd $ foldr f (True, mempty) [1 .. n + 1]
  rhs2 = snd $ foldr g (0 :: Int, mempty) [1 .. n + 1]
  f :: Int -> (Bool, Text) -> (Bool, Text)
  f i (b, z) =
    let p = i - 1
     in if b
          then (False, [st|MZ (fmap (\(MZ lst#{i}) -> lst#{i}) lst#{p})#{z}|])
          else (b, [st|MS (fmap (\(MS lst#{i}) -> #{z}) lst#{p})|])
  g :: Int -> (Int, Text) -> (Int, Text)
  g i (b, z) =
    let p = i - 1
     in let inner = [st|MS (fmap MZ lst#{p})|]
            _MZMS = bool @String "MS" "MZ" (b == 1)
         in if b == 0
              then (b + 1, inner) -- else (False,[st|MS (fmap (\(MZ lst#{i}) -> #{inner}) lst#{p})#{z}|])
              else (b + 1, [st|MS (fmap (\(#{_MZMS} lst#{i}) -> #{z}) lst#{p})|])

ituples :: Int -> String
ituples n = L.foldr (\i z -> wrapParens ("a" ++ show i ++ ", " ++ z)) "()" [1 .. n]

flattuples :: Int -> String
flattuples n = L.intercalate ", " $ map (('a' :) . show) [1 .. n]

-- mapM_ (putStrLn . genToITupleT) (positiveRange' 2 10) -- to generate from two onwards
genToITupleT :: Positive -> String
genToITupleT (unPositive -> n)
  | n == 1 = error "genToITupleT: special case using One: you're own your own"
  | otherwise =
      T.unpack
        [st|  ToITupleT (#{flattuples n}) = #{ituples n}|]

-- mapM_ (putStrLn . genFromITupleT) (positiveRange' 2 10) -- to generate from two onwards
genFromITupleT :: Positive -> String
genFromITupleT (unPositive -> n)
  | n == 1 = error "genFromITupleT: special case using One: you're own your own"
  | otherwise =
      T.unpack
        [st|  FromITupleT #{ituples n} = (#{flattuples n})|]

-- mapM_ (putStrLn . genToITupleC) (positiveRange' 2 10) -- to generate from two onwards
genITupleC :: Positive -> String
genITupleC (unPositive -> n)
  | n == 1 = error "genToITupleT: special case using One: you're own your own"
  | otherwise =
      T.unpack
        [st|instance ITupleC (#{flattuples n}) where
  toITupleC (#{flattuples n}) = #{ituples n}
  fromITupleC #{ituples n} = (#{flattuples n})|]

genITupleAll :: Positive -> String
genITupleAll n =
  let xs = toList $ positiveRange' 2 (unPositive n)

      a1 =
        T.unpack
          [st|type ToITupleT :: Type -> Type
type family ToITupleT x = result | result -> x where
  ToITupleT (One a1) = (a1, ())
|]

      a2 = unlines $ map genToITupleT xs

      b1 =
        T.unpack
          [st|type FromITupleT :: Type -> Type
type family FromITupleT x = result | result -> x where
  FromITupleT (a1, ()) = One a1
|]

      b2 = unlines $ map genFromITupleT xs

      c1 =
        T.unpack
          [st|type ITupleC :: Type -> Constraint
class ITupleC x where
  toITupleC :: x -> ToITupleT x
  fromITupleC :: ToITupleT x -> x
instance ITupleC (One a1) where
  toITupleC (One a1) = (a1, ())
  fromITupleC (a1, ()) = One a1
|]

      c2 = unlines $ map genITupleC xs
   in a1 ++ a2 ++ "\n" ++ b1 ++ b2 ++ "\n" ++ c1 ++ c2
