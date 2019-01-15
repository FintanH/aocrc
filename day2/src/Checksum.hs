{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

module Checksum where

import           Control.Monad (guard)
import           Data.Bifunctor (bimap)
import           Data.Foldable (maximumBy)
import qualified Data.Map as Map
import           Data.Monoid (Sum (..))
import           Data.Ord (comparing)

import           Yaya.Fold
import           Yaya.Pattern

import           Yaya.Unsafe.Fold.Instances ()

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

searchSum :: Num a => Search -> (Sum a, Sum a)
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

searchStringAlg :: XNor Char (Map.Map Char (Int, Search) -> Map.Map Char (Int, Search))
                -> Map.Map Char (Int, Search) -> Map.Map Char (Int, Search)
searchStringAlg = \case
  Neither -> id
  Both c s -> \m ->
    case Map.lookup c m of
      Nothing -> s $ Map.insert c (1, mempty) m
      Just (i, search) ->
        let j = i + 1
            search' = search <> toSearch j
         in s $ Map.insert c (j, search') m

searchString :: String -> Search
searchString = foldMap toSearch . foldr (\c -> Map.insertWith (+) c 1) Map.empty

checksum :: String -> Integer
checksum = f . foldMap searchSum . map searchString . lines
  where
    f (Sum n, Sum m) = n * m

checksum' :: String -> Integer
checksum' = f
          . mconcat
          . (map snd . Map.elems)
          . ($ Map.empty)
          . mconcat
          . map (fmap (fmap . fmap $ searchSum) . cata searchStringAlg)
          . lines
  where
    f (Sum n, Sum m) = n * m

hammingDistance :: String -> String -> String
hammingDistance s1 s2 = map fst . filter snd $ zipWith (\c c' -> (c,c == c')) s1 s2

checkIds :: String -> String
checkIds s = maximumBy (comparing length) $ do
  let ids = zip [1 :: Int ..] (lines s)
  (i, t) <- ids
  (j, t') <- ids
  guard $ i /= j
  pure $ hammingDistance t t'
