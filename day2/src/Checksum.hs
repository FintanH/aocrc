{-# LANGUAGE LambdaCase #-}

module Checksum where

import           Data.Bifunctor (bimap)
import           Data.Bool (bool)
import qualified Data.Map as Map
import           Data.Monoid (Sum (..))

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

hammingDistance :: String -> String -> Int
hammingDistance s1 s2 = getSum . foldMap (bool mempty (Sum 1)) $ zipWith (==) s1 s2

checkIds :: String -> Int
checkIds s =
  let s'@(_:s'') = lines s
  in minimum . map (uncurry hammingDistance) $ zip s' s''
