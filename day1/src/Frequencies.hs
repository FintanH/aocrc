{- | Notes:
        e.g. (r ->), State, Either, etc.
      As you begin to wish for functionality then you wish things into existence,
      Begin by writing algebra, base case, recursive cases
      Recursive instance of XNor and [], unsafe to due infinite lists
      Start with sum pattern functor
      backwards
      cata works from the leaves upwards, so in the XNor case we will process the list

      recursion schemes help you suss out bad specifications ;) see repeatConcat
 -}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

module Frequencies where

import Control.Applicative ((<|>))
import Control.Monad.Except
import Control.Monad.State

import Data.Attoparsec.ByteString
import Data.Attoparsec.Char8 (char, digit, endOfLine)
import Data.ByteString (ByteString)
import Data.Foldable
import Data.Functor.Const
import Data.Set (Set)
import qualified Data.Set as Set

import Yaya
import Yaya.Control
import Yaya.Data
import Yaya.Either
import Yaya.Unsafe.Data
import Yaya.Zoo

import Debug.Trace

plus :: Parser Char
plus = char '+'

minus :: Parser Char
minus = char '-'

number :: Parser Int
number = do
  sign <- try (minus >> pure negate) <|> (plus >> pure id)
  sign . read <$> many1 digit

numbers :: Parser [Int]
numbers = number `sepBy` endOfLine

sum' :: Algebra (XNor Int) Int
sum' = \case
  Both x y -> x + y
  None     -> 0

repeatedFrequency :: XNor Int Int -> State (Set Int) Int
repeatedFrequency = \case
  Both x y -> do
    env <- get
    let r = y + x
    if r `Set.member` env
       then pure r
       else put (r `Set.insert` env) >> (pure r)
  None -> pure 0

-- Coalgebra ((,) a) (t, t) === (t, t) -> (a, (t, t))
-- AndMaybe is the pattern functor for NonEmpty
repeatConcat :: Steppable t (AndMaybe a) => Coalgebra ((,) a) (t, t)
repeatConcat (orig, current) =
  case project current of
    Only a     -> (a, (orig, orig))
    Indeed a t -> (a, (orig, t))

-- | Create a non-empty sequence by consing an element onto a sequence
-- Algebra (XNor a) (a -> t) === XNor a (a -> t) -> (a -> t)
nonEmpty :: Steppable t (AndMaybe a) => Algebra (XNor a) (a -> t)
nonEmpty = \case
  None -> embed . Only
  Both a f -> embed . flip Indeed (f a)

makeStream :: List Int -> Stream Int
makeStream l = case project l of
  None     -> ana duplicate 0
  Both h t -> ana repeatConcat (duplicate $ (cata nonEmpty t h :: Mu (AndMaybe Int)))
  where
    duplicate :: Coalgebra ((,) a) a
    duplicate i = (i, i)

-- -6, +3, +8, +5, -6
--  , -6, +3, +8, +5, -6,
-- 0, -6, -3, 5, 10, 4, -2, 1, 9, 14, 8, 2, 5
bar :: Coalgebra (Either Int) (Int, Set Int, Stream Int)
bar (sum, env, stream) =
  let (currentElem, restOfStream) = project stream
      result = sum + currentElem
  in if result `Set.member` env
     then Left result
     else Right $ (result, Set.insert result env, restOfStream)

foo :: XNor Int Int -> State (Set Int) (Either Int Int)
foo = \case
  Both currentElement sum -> do
    let result = currentElement + sum
    env <- get
    if result `Set.member` env
       then pure $ Left result
       else put (Set.insert result env) >> pure (Right result)
  None -> pure (Right 0)

frequency :: ByteString -> Int
frequency = either (error . show) id . fmap (cata sum') . parseOnly numbers

runComp :: [Int] -> Int
runComp = either id (error "Could not find frequency")
        . flip evalState (Set.singleton 0) . runExceptT . cataM (ExceptT . foo)

runner :: [Int] -> Nu (Either Int)
runner ns = ana bar (0, Set.singleton 0, makeStream $ cata embed ns)

frequency' :: ByteString -> Int
frequency' = either (error . show) id
           -- . fmap (($ Set.singleton 0) . cata foo . reverse)
           . fmap (runToEnd . runner)
           . parseOnly numbers
