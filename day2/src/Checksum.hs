{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeApplications  #-}

module Checksum where

import           Control.Monad (guard)
import           Data.Bifunctor (bimap)
import           Data.Foldable (maximumBy)
import qualified Data.Map as Map
import           Data.Monoid (Sum (..))
import           Data.Ord (comparing)

import           Yaya.Control
import           Yaya.Data

import           Yaya.Unsafe.Data ()

-- input is a string of characters followed by a newline
-- need to map-reduce count of repeating letters
-- if we have a 2 repeat (one or more) or 3 repeat (one or more) tally these
-- multiply tally to get final checksum

--
data Find
  = NotFound
  | Found

instance Semigroup Find where
  Found <> _ = Found
  _ <> Found = Found
  f <> _     = f

instance Monoid Find where
  mempty = NotFound
  mappend = (<>)

type Search = (Find, Find)

searchSum :: Search -> (Sum Integer, Sum Integer)
searchSum = bimap toSum toSum
  where
    toSum :: Num a => Find -> Sum a
    toSum = \case
      NotFound -> mempty
      Found    -> Sum 1

toSearch :: Int -> Search
toSearch 2 = (Found, NotFound)
toSearch 3 = (NotFound, Found)
toSearch _ = mempty

countAlg :: XNor Char (Map.Map Char Int) -> Map.Map Char Int
countAlg = \case
  Neither  -> Map.empty
  Both c m -> Map.insertWith (+) c 1 m

searchStringAlg :: XNor Char (Map.Map Char Int -> Search) -> (Map.Map Char Int -> Search)
searchStringAlg = \case
  Neither -> const mempty
  Both c s -> \m ->
    let (mV, m') = Map.insertLookupWithKey (\_ i j -> i + j) c 1 m
    in maybe (s m') (\v -> s m' <> toSearch (v + 1)) mV

toSearchAlg :: XNor (Char, Int) Search -> Search
toSearchAlg = \case
  Neither       -> mempty
  Both (_, i) s -> toSearch i <> s

toSumAlg :: XNor Search (Sum Integer, Sum Integer) -> (Sum Integer, Sum Integer)
toSumAlg = \case
  Neither   -> mempty
  Both s s' -> searchSum s <> s'

searchString :: String -> Search
searchString = foldMap toSearch . foldr (\c -> Map.insertWith (+) c 1) Map.empty

checksum :: String -> Integer
checksum = f . foldMap searchSum . map searchString . lines
  where
    f (Sum n, Sum m) = n * m

checksum' :: String -> Integer
checksum' = f
          . cata toSumAlg
          . map (cata toSearchAlg . Map.toList . cata countAlg)
          . lines
  where
    f (Sum n, Sum m) = n * m

hammingAlg :: Eq a => XNor (a, a) ([a] -> [a]) -> ([a] -> [a])
hammingAlg = \case
  Neither -> id
  Both (a, b) acc -> \l ->
    let r = acc l
    in if a == b then (a:r) else r

hammingDistance :: String -> String -> String
hammingDistance s1 s2 = map fst . filter snd $ zipWith (\c c' -> (c,c == c')) s1 s2

hammingDistanceBetter :: [Char] -> [Char] -> [Char]
hammingDistanceBetter [] [] = []
hammingDistanceBetter _  [] = []
hammingDistanceBetter [] _  = []
hammingDistanceBetter (c:cs) (c':cs') =
  if c == c'
     then c:hammingDistanceBetter cs cs'
     else hammingDistance cs cs'

checkIds :: String -> String
checkIds s = maximumBy (comparing length) $ do
  let ids = zip [1 :: Int ..] (lines s)
  (i, t) <- ids
  (j, t') <- ids
  guard $ i /= j
  pure $ hammingDistanceBetter t t'

checkIds' :: String -> String
checkIds' s = maximumBy (comparing length) $ do
  let ids = zip [1 :: Int ..] (lines s)
  (i, t) <- ids
  (j, t') <- ids
  guard $ i /= j
  pure $ cata hammingAlg (zip t t') []
