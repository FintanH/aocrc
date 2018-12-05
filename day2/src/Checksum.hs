
module Checksum where

import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- input is a string of characters followed by a newline
-- need to map-reduce count of repeating letters
-- if we have a 2 repeat (one or more) or 3 repeat (one or more) tally these
-- multiply tally to get final checksum

data Found
  = Two
  | Three
  | Both
  | None

instance Monoid Foud where
  None <> f = f
  f <> None = f
  Both <> _ = Both
  _ <> Both = Both
  Two <> Three = Both
  Three <> Two = Both
  f <> _ = f

foo :: Int -> Found -> Found
foo 2 Two = Two
foo 2 None = Two
foo 2 Three = Both
foo 2 Both = Both

foo 3 None = Three
foo 3 Three = Three
foo 3 Two = Both
foo 3 Both = Both

foo _ None = None
foo _ f = f

count :: String -> Map Char Int
count = foldr (\c -> Map.insertWith (+) c 1) Map.empty

buckets :: Map Char Int -> Found
buckets = foldr foo None
