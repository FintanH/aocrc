
module Checksum where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid (Sum (..))
import qualified Data.Set as Set

-- input is a string of characters followed by a newline
-- need to map-reduce count of repeating letters
-- if we have a 2 repeat (one or more) or 3 repeat (one or more) tally these
-- multiply tally to get final checksum

data Find
  = NotFound
  | Found

instance Semigroup Find where
  Found <> _ = Found
  _ <> Found = Found
  f <> _     = f

data Search = Search
  { twos :: Find, threes :: Find }

instance Semigroup Search where
  Search twos' threes' <> Search twos'' threes'' =
    Search (twos' <> twos'') (threes' <> threes'')

searchSum :: Search -> (Sum Integer, Sum Integer)
searchSum (Search twos_ threes_) =
  case (twos_, threes_) of
    (NotFound, NotFound) -> (mempty, mempty)
    (Found,    NotFound) -> (Sum 1, mempty)
    (NotFound, Found)    -> (mempty, Sum 1)
    (Found,    Found)    -> (Sum 1, Sum 1)

searchString :: String -> Search
searchString = searchIt . Map.updateWith (+1)
  where
    searchIt :: Map Char Int -> Search
    searchIt = foldr (\i s ->  if i == 2 then Search Found NotFound <> s
                          else if i == 3 then Search NotFound Found <> s
                          else s) (Search NotFound NotFound)


searchStrings :: [String] -> [Search]
searchStrings = undefined

checksum :: [Search] -> Integer
checksum = f . foldMap searchSum
  where
    f (Sum n, Sum m) = n * m
