{-# LANGUAGE LambdaCase #-}

module Checksum where

import           Control.Monad (guard)
import           Data.Bifunctor (bimap)
import           Data.Foldable (maximumBy)
import qualified Data.Map as Map
import           Data.Monoid (Sum (..))
import           Data.Ord (comparing)

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

searchString :: String -> Search
searchString = foldMap toSearch . foldr (\c -> Map.insertWith (+) c 1) Map.empty
  where
    toSearch :: Int -> Search
    toSearch 2 = (Found, NotFound)
    toSearch 3 = (NotFound, Found)
    toSearch _ = mempty

checksum :: String -> Integer
checksum = f . foldMap searchSum . map searchString . lines
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
