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
import Data.ByteString (ByteString)
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Parser.Char (char, digit, newline)

import Yaya
import Yaya.Control
import Yaya.Data
import Yaya.Either
import Yaya.Unsafe.Data () -- imported for orphan instances
import Yaya.Zoo

-- | Parse a `+` token.
plus :: Parser Char
plus = char '+'

-- | Parse a `-` token.
minus :: Parser Char
minus = char '-'

-- | Parse a sign (+/-) followed by its digits.
number :: Parser Int
number = do
  sign <- try (minus >> pure negate) <|> (plus >> pure id)
  sign . read <$> many1 digit

-- | Parse a newline separated string of 'number'.
numbers :: Parser [Int]
numbers = number `sepBy` newline

-- | This 'Algebra' serves as the summing of our incoming frequencies.
--
--   XNor acts as our "pattern functor" which is the pattern functor
--   for lists, i.e. [a].
--
--   We can see this by looking at the definition of 'XNor' from yaya.
--   @
--     data XNor a b
--       = None
--       | Both a b
--   @
--
--   The case of 'None' acts the base case '[]' (or Nil).
--   The case of 'Both' acts as 'a : [a]' (or Cons). It contains
--   the item in the list and the rest of the list.
--
--   'Algebra f a' is an alias for 'f a -> a', so we can
--   translate the type signature to 'XNor Int Int -> Int'.
--
--   'Both x' is the head of the list, our current element
--   the 'y' being the accumulated sum, from the tail.
--
--   'None', as mentioned is the base case and thus we can
--   return 0.
--
--   This 'Algebra' can then be passed to 'cata' which will
--   process things from the back of the list, (read leaf of computation)
--   up to towards the head.
sum' :: Algebra (XNor Int) Int
sum' = \case
  Both x y -> x + y
  None     -> 0

-- Coalgebra ((,) a) (t, t) === (t, t) -> (a, (t, t))
-- AndMaybe is the pattern functor for NonEmpty
repeatConcat :: Coalgebra ((,) a) (Mu (AndMaybe a), Mu (AndMaybe a))
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
  Both h t -> ana repeatConcat (duplicate $ (cata nonEmpty t h))
  where
    duplicate :: Coalgebra ((,) a) a
    duplicate i = (i, i)

repeatCoalgebra :: Coalgebra (Either Int) (Int, Set Int, Stream Int)
repeatCoalgebra (tally, env, stream) =
  let (currentElem, restOfStream) = project stream
      result = tally + currentElem
  in if result `Set.member` env
     then Left result
     else Right $ (result, Set.insert result env, restOfStream)

repeatAlgebra :: XNor Int Int -> State (Set Int) (Either Int Int)
repeatAlgebra = \case
  Both currentElement tally -> do
    let result = currentElement + tally
    env <- get
    if result `Set.member` env
       then pure $ Left result
       else put (Set.insert result env) >> pure (Right result)
  None -> pure (Right 0)

frequency :: ByteString -> Int
frequency = either (error . show) id . fmap (cata sum') . parseOnly numbers

runRepeat :: [Int] -> Int
runRepeat = either id (error "Could not find frequency")
        . flip evalState (Set.singleton 0) . runExceptT . cataM (ExceptT . repeatAlgebra)

findRepeat :: ByteString -> Int
findRepeat = either (error . show) id
           . fmap (runToEnd . runner)
           . parseOnly numbers
  where
    runner :: [Int] -> Nu (Either Int)
    runner ns = ana repeatCoalgebra (0, Set.singleton 0, makeStream $ cata embed ns)

