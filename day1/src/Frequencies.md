{-[]{# LANGUAGE FlexibleContexts  .anchor}-}
{-[]{# LANGUAGE LambdaCase        .anchor}-}
{-[]{# LANGUAGE TypeApplications  .anchor}-}

module Frequencies where

import Control.Applicative (([\|](| "|"))) import Control.Monad.Except
import Control.Monad.State

import Data.Attoparsec.ByteString import Data.ByteString (ByteString)
import Data.Set (Set) import qualified Data.Set as Set import
Text.Parser.Char (char, digit, newline)

import Yaya import Yaya.Control import Yaya.Data import Yaya.Either
import Yaya.Unsafe.Data () \-- imported for orphan instances import
Yaya.Zoo

{- \| If you hang around fellow Haskeller/Scala head, Greg Pfeil (a.k.a
sellout), enough you will hear about recursion schemes, as well as some
other abstract concepts but those are not the topic of the day. No,
today we will join the conversation and talk about recursion schemes as
well.

I have dabbled with the concept for a long time. Due to being a
colleague of Greg\'s at
[Formation](https://formation.ai/ "https://formation.ai/") I had to
familiarise myself somewhat with the concepts. But what annoyed me was
that I didn\'t always get a full understanding of it. I could be given a
jumping off point to work with, but could never arrive at the initial
ideas myself.

On the lookout for the perfect opportunity to add some skill points to
my recursion shcemes knowledge I decided to tackle the [Advent of
Code](https://adventofcode.com/ "https://adventofcode.com/") challenges
but force myself to use recursion schemes. Granted I am very slow at
solving these problems, but at least I\'m not in it for the competition.

So I started Day 1, armed with the
[yaya](https://github.com/sellout/yaya "https://github.com/sellout/yaya")
library, my point of contact, \@sellout, some tunes on Spotify, and the
thirst for knowledge.

The functions below are heavily talked about and explained in detail as
to what the types involve and how the implementations work. Before that
though, we will discuss here on how we should start to think about
recursion schemes.

This section will basically be the lessons gleaned from talking to Greg
about how to organise a recursion schemes workflow, a few terms, and
some general Good Things(TM). Of course we start with the problem we are
trying to solve. We have some spec of a thing we are trying implement
and from this we must design an algorithm that will talk about how this
problem will be solved. Here we will see the first benefit of recursion
schemes, that is talking purely about the algorithm itself. We will take
the spec and go on a case by case business and compute what we need at
each step, but we won\'t worry about the recursion itself!

The idea of talking just about the computation is ushered in by choosing
a pattern functor. Below we will see a grab-bag of pattern functors and
we will get a better feel for them as we go along. As we solve more
problems hopefully choosing pattern functors becomes a more natural
process. The pattern functor will be, as you may have guessed, a
functor. The computation will then fall out of the cases you need to
match on for your functor. This implementation will be most commonly
referred to as an \"algebra\" or a \"coalgebra\". As we are writing the
algebra we will generally have some assumptions about our spec, but as
we go along we will most likely see cases where we need to add power.
Some examples of this are, introducing a `Set`{.haskell .identifier} to
keep a record of visited values, using `State`{.haskell .identifier} to
modify the visited values, using \`(-\>)\` to pass an environment, using
`Either`{.haskell .identifier} to short circuit computation. So the
moral of this point is to start with some assumption, a pattern functor,
and modify your algebra as necessities arise, but once this is writter
there should not be much a need to modify it in the next stages, i.e.
folds and unfolds.

The next idea is that we should decompose our problem(s) into many
algebras. Each algebra should do one thing and one thing only. As
computer scientists*engineers*programmers we strive to do this a lot and
often, so that can reason about smaller parts. I would argue that
algebras force us to do this as often as possible because our
computations will always be localised to the algebra itself. As
functional programmers we strive to compose things, taking our smaller
parts and creating larger pieces that we can reason about due to knowing
how the smaller pieces work.

From our algebras we can then interpret them by running them through
folds, such as `cata`{.haskell .identifier} and unfolds, such as
`ana`{.haskell .identifier}. The fact that that\'s pretty much all I had
to say about the folds is a testament of how useful it is to think about
your algebra first and foremost and then your folds and unfolds come
after. Akin to \"Implement first and optimise later\".

Without further ado, lets see how we can solve Day 1 of AOC with
recursion schemes! -}

-   \- \| Parse a `+`{.haskell .identifier} token. plus :: Parser Char
    plus = char `+`{.haskell .identifier}

-   \- \| Parse a `-`{.haskell .identifier} token. minus :: Parser Char
    minus = char `-`{.haskell .identifier}

-   \- \| Parse a sign (+/-) followed by its digits. number :: Parser
    Int number = do sign [try (minus](- "-")\> pure negate) [\|](| "|")
    (plus \>\> pure id) sign . read [\$]($ "$") many1 digit

-   \- \| Parse a newline separated string of `number`{.haskell
    .identifier}. numbers :: Parser \[Int\] numbers = number
    `sepBy`{.haskell .identifier} newline

-   \- \| This `Algebra`{.haskell .identifier} serves as the summing of
    our incoming frequencies.

-   \-

-   \- XNor acts as our \"pattern functor\" which is the pattern functor

-   \- for lists, i.e. \[a\].

-   \-

-   \- We can see this by looking at the definition of `XNor`{.haskell
    .identifier} from yaya.

-   \- @

-   \- data XNor a b

-   \- = None

-   \- \| Both a b

-   \- @

-   \-

-   \- The case of `None`{.haskell .identifier} acts the base case
    \'\[\]\' (or Nil).

-   \- The case of `Both`{.haskell .identifier} acts as \'a : \[a\]\'
    (or Cons). It contains

-   \- the item in the list and the rest of the list.

-   \-

-   \- \'Algebra f a\' is an alias for \'f a -\> a\', so we can

-   \- translate the type signature to \'XNor Int Int -\> Int\'.

-   \-

-   \- \'Both x\' is the head of the list, our current element

-   \- the `y`{.haskell .identifier} being the accumulated sum, from the
    tail.

-   \-

-   \- `None`{.haskell .identifier}, as mentioned is the base case and
    thus we can

-   \- return 0.

-   \-

-   \- This `Algebra`{.haskell .identifier} can then be passed to
    `cata`{.haskell .identifier} which will

-   \- process things from the back of the list, (read leaf of
    computation)

-   \- up to towards the head. sum\' :: Algebra (XNor Int) Int sum\' =
    case Both x y -\> x + y None -\> 0

-   \- \| This `Coalgebra`{.haskell .identifier} serves as taking a
    `NonEmptyList`{.haskell .identifier} and repeating it, followed

-   \- by concatenating them together.

-   \-

-   \- This can be visualised as:

-   \- \* Given a list: 1, 2, 3, 4

-   \- \* Results in: 1, 2, 3, 4, 1, 2, 3, 4, \...

-   \-

-   \- \'NonEmptyList a\' is a type alias for \'Mu (AndMaybe a)\'. There
    are two things

-   \- we need to address here, so let\'s talk about `AndMaybe`{.haskell
    .identifier} first.

-   \-

-   \- `AndMaybe`{.haskell .identifier} is our pattern functor and acts
    as, you guessed it, the pattern functor

-   \- for non-empty lists. We can understand this by looking at its
    definition:

-   \- @

-   \- data AndMaybe a b

-   \- = Only a

-   \- \| Indeed a b

-   \- @

-   \-

-   \- We can read this as \'a `AndMaybe`{.haskell .identifier} b\', so
    we are always guaranteed an `a`{.haskell .identifier}

-   \- and possibly a `b`{.haskell .identifier}.

-   \-

-   \- We have a guaranteed item in `Only`{.haskell .identifier}, and
    then `Indeed`{.haskell .identifier} looks similar

-   \- to `Both`{.haskell .identifier} from `XNor`{.haskell
    .identifier}, so the head, and the rest of the list.

-   \-

-   \- A usual construction of NonEmpty is that we have a guaranteed
    `head`{.haskell .identifier} of the

-   \- list and the rest of the list. In this case `Only`{.haskell
    .identifier} acts as the guaranteed `last`{.haskell .identifier}

-   \- of the list, and `Indeed`{.haskell .identifier} as the front
    (`init`{.haskell .identifier}) of the list.

-   \-

-   \- We have now introduced `Mu`{.haskell .identifier} as well. Let\'s
    gain some intuition for what it is

-   \- by saying that the type alias \'List a\' can be defined as \'Mu
    (XNor a)\'.

-   \-

-   \- `Mu`{.haskell .identifier} is the fixed-point operator for
    finite/inductive data structures. This means

-   \- we can build up a non-empty list, embedding our pattern functors
    in `Mu`{.haskell .identifier} and this

-   \- expresses that we have a finite computation.

-   \-

-   \- At this point we can unpack what our type signature means:

-   \- @

-   \- Coalgebra ((,) a) (NonEmptyList a, NonEmptyList a)

-   \- === (NonEmptyList a, NonEmptyList a) -\> (a, (NonEmptyList a,
    NonEmptyList a))

-   \- @

-   \-

-   \- So we have the original list as our first element in the \'(,)\',
    and the

-   \- current `NonEmptyList`{.haskell .identifier} that we are
    inspecting. Calling `project`{.haskell .identifier} lets

-   \- us unwrap one layer `Mu`{.haskell .identifier} and gives us back
    an `AndMaybe`{.haskell .identifier} value that we case on.

-   \-

-   \- If we have `Only`{.haskell .identifier} one item we return the
    last item of the list and the original

-   \- list again, i.e. we\'ve reached the end so now we repeat.

-   \-

-   \- If we have `Indeed`{.haskell .identifier} then we unpack the head
    and pass down the tail.

-   \-

-   \- We always keep the original to refer to it once we get to the end
    of the list. repeatConcat :: Coalgebra ((,) a) (NonEmptyList a,
    NonEmptyList a) repeatConcat (orig, current) = case project current
    of Only a -\> (a, (orig, orig)) Indeed a t -\> (a, (orig, t))

-   \- \| Create a non-empty sequence by consing an element onto a
    sequence

-   \- \| Here we are creating an `Algebra`{.haskell .identifier} to
    convert a list to a non-empty list.

-   \-

-   \- As we saw in `sum\'`{.haskell .identifier} we were representing a
    list through its pattern functor

-   \- `XNor`{.haskell .identifier}, which we also notice here.

-   \-

-   \- And in `repeatConcat`{.haskell .identifier} we looked through
    `NonEmptyList`{.haskell .identifier} and how it was an

-   \- alias for \'Mu (AndMaybe a)\' our pattern functor for non-empty
    lists and

-   \- the fixed-point operator for finite data structures.

-   \-

-   \- So lets look at what this `Algebra`{.haskell .identifier} breaks
    out into:

-   \- @

-   \- Algebra (XNor a) (a -\> NonEmptyList a)

-   \- === XNor a (a -\> NonEmptyList a) -\> (a -\> NonEmptyList a)

-   \- @

-   \-

-   \- So, given an `XNor`{.haskell .identifier} of an `a`{.haskell
    .identifier}s as the items, and \'a -\> NonEmptyList a\'

-   \- as the tail or continuation of the list, we get back a function
    from

-   \- \'a -\> NonEmptyList a\'.

-   \-

-   \- This comes across as intuitive, since we can turn any list into a

-   \- non-empty list as long as provide at lease ONE item. Our initial
    `a`{.haskell .identifier}

-   \- for the function!

-   \-

-   \- In the case of `None`{.haskell .identifier}, our function will be
    `Only`{.haskell .identifier}. So we take that

-   \- `a`{.haskell .identifier} passed in and return the singleton,
    non-empty list.

-   \-

-   \- In the case of `Both`{.haskell .identifier}, we have the head of
    our list, and the `NonEmptyList`{.haskell .identifier}

-   \- being built up from our `Only`{.haskell .identifier} element. So,
    we apply the continuation and

-   \- it now acts as the head of our `Indeed`{.haskell .identifier}
    non-empty list. Again, waiting

-   \- for the next continuation, when we finally call this function.
    nonEmpty :: Algebra (XNor a) (a -\> NonEmptyList a) nonEmpty = case
    None -\> embed . Only Both a f -\> embed . flip Indeed (f a)

-   \- \| This is what it all comes down to. We want to turn our list
    into

-   \- a repeating stream of elements.

-   \-

-   \- Our input is \'List Int\' which is a type alias for \'Mu (XNor
    a), which

-   \- at this point we know by now that combining the pattern functor
    for

-   \- lists, `XNor`{.haskell .identifier}, and `Mu`{.haskell
    .identifier} we are saying we have a finite list structure.

-   \-

-   \- Our output is \'Stream Int\', which is a type alias for \'Nu ((,)
    a).

-   \- This particular pattern we have not come across yet. So lets see
    what

-   \- these two things combine entail.

-   \-

-   \- `Nu`{.haskell .identifier} is the dual fixed-point operator to
    `Mu`{.haskell .identifier}. As its dual it means

-   \- that it describes infinite/co-inductive data structures. So when
    we

-   \- combine `Nu`{.haskell .identifier} with pattern functors, we are
    saying that we have potentially

-   \- infinite data. This is very true for our case of repeating a
    list.

-   \-

-   \- \'(,) a\' is chosen as the pattern functor because it naturally
    represents

-   \- an infinite stream of data. We have the head of the stream, the
    first element

-   \- of the tuple, and the rest of the stream, the second element of
    the stream.

-   \-

-   \- We could anaolgise this to a manual recursive definition of a
    stream:

-   \- @

-   \- data Stream = a :\<\< Stream a

-   \- @

-   \-

-   \- Another way of looking at it is streams are lists without a Nil
    case.

-   \-

-   \- With this explanation out of the way we can break down how to
    implement this.

-   \- We first `project`{.haskell .identifier} our list `l`{.haskell
    .identifier} to unwrap one layer of our `List`{.haskell
    .identifier}.

-   \-

-   \- In the case of `None`{.haskell .identifier} we make a \"safe\"
    move of producing a stream of 0s.

-   \- We cannot do much with an empty list, since it is undefined for
    `Stream`{.haskell .identifier} data,

-   \- but it works in our larger problem because adding 0 is no-op.

-   \-

-   \- In the case of `Both`{.haskell .identifier} we utilise our
    `nonEmpty`{.haskell .identifier} algebra to convert our
    `List`{.haskell .identifier}

-   \- into a `NonEmptyList`{.haskell .identifier}. We duplicate this
    result and infinitely generate, via `ana`{.haskell .identifier},

-   \- a `Stream`{.haskell .identifier} of `NonEmptyList`{.haskell
    .identifier}s using `repeatConcat`{.haskell .identifier}. makeStream
    :: List Int -\> Stream Int makeStream l = case project l of None -\>
    ana duplicate 0 Both h t -\> ana repeatConcat (duplicate \$ (cata
    nonEmpty t h)) where duplicate :: Coalgebra ((,) a) a duplicate i =
    (i, i)

-   \- \| This is the meat of our problem. Here we are implementing the

-   \- algorithm to detect repeating frequencies.

-   \-

-   \- The problem at hand is that given a repeating stream of integers

-   \- we should keep a tallying sum and as soon as we see the first
    repeat

-   \- we should return that result.

-   \-

-   \- An example given on the site is:

-   \- Input: -6, +3, +8, +5, -6

-   \- Output: 5

-   \- Progress: 0, -6, -3, 5, 10, 4, -2, 1, 9, 14, 8, 2, 5

-   \-

-   \- Since we don\'t necessarily know how long this algorithm can go
    on

-   \- for we describe it as `Coalgebra`{.haskell .identifier} and
    provide it a seed. So lets

-   \- inspect the seed first.

-   \-

-   \- Our input is \'(Int, Set Int, Stream Int)\'. The first value is
    our

-   \- running tally, so we can keep an account of our sum as go
    progress.

-   \- \'Set Int\' keeps track of the values we have seen so far.
    Finally,

-   \- \'Stream Int\' is the repeating stream that we will be inspecting

-   \- as we progress.

-   \-

-   \- The first thing we want to do is inspect the head of the stream,
    this

-   \- can be done by using `project`{.haskell .identifier} to split the
    stream in its head and its

-   \- tail.

-   \-

-   \- We can then get our latest tally by summing our current element,
    the head

-   \- of the stream, with our aggregated tally, the seed.

-   \-

-   \- We then check that our `result`{.haskell .identifier} is a member
    of the `Set`{.haskell .identifier} carried in the seed.

-   \- If it is, then great! We found our result and shortcircuit with
    `Left`{.haskell .identifier}.

-   \- If not, we continue the combination updating our seed values, and
    returning

-   \- with `Right`{.haskell .identifier}.

-   \-

-   \- Lets take a moment to consider the semantics of `Either`{.haskell
    .identifier} in this case, because

-   \- it\'s important to note how its acting and its general use in
    recursion schemes.

-   \-

-   \- When we think about `Either`{.haskell .identifier} we generally
    think about short-circuiting behaviour.

-   \- This is drilled into us when we write the `Functor`{.haskell
    .identifier}, `Applicative`{.haskell .identifier}, and
    `Monad`{.haskell .identifier} instances.

-   \- When we see the `Left`{.haskell .identifier} case we propagate
    this result.

-   \- In our case it\'s the natural pattern functor for partial
    functions. This can be seen in the

-   \- newtype defined in yaya, `Partial`{.haskell .identifier}. It is
    defined as \'Partial a = Nu (Either a)\'.

-   \- The `Partial`{.haskell .identifier} relates to the short
    circuiting. We \_may\_ get a result and this will be

-   \- passed back as a `Left`{.haskell .identifier} value. Or we may
    just keep computing forever with a `Right`{.haskell .identifier}
    value.

-   \-

-   \- In our case this is very true since we could have a list of one
    number `+1`{.haskell .identifier}. This

-   \- will end up in a stream of infinite `+1`{.haskell .identifier}s
    and it will never find a repeating frequency! repeatCoalgebra ::
    Coalgebra (Either Int) (Int, Set Int, Stream Int) repeatCoalgebra
    (tally, env, stream) = let (currentElem, restOfStream) = project
    stream result = tally + currentElem in if result
    `Set.member`{.haskell .identifier} env then Left result else Right
    \$ (result, Set.insert result env, restOfStream)

-   \- \| We can leave this as an exercise to the reader of what the
    attempt

-   \- was here, and why it doesn\'t work. repeatAlgebra :: XNor Int Int
    -\> State (Set Int) (Either Int Int) repeatAlgebra = case Both
    currentElement tally -\> do let result = currentElement + tally env
    \<- get if result `Set.member`{.haskell .identifier} env then pure
    \$ Left result else put (Set.insert result env) \>\> pure (Right
    result) None -\> pure (Right 0)

-   \- \| This will give an answer to part 1 of the challenge: read in a
    list

-   \- of numbers in the format \`+/-\[1-9\]+\` and sum them. frequency
    :: ByteString -\> Int frequency = either (error . show) id . fmap
    (cata sum\') . parseOnly numbers

-   \- \| Helper for using the `repeatAlgebra`{.haskell .identifier}.
    runRepeat :: \[Int\] -\> Int runRepeat = either id (error \"Could
    not find frequency\") . flip evalState (Set.singleton 0) .
    runExceptT . cataM (ExceptT . repeatAlgebra)

-   \- \| This is the helper to run our `repeatCoalgebra`{.haskell
    .identifier}. It uses `ana`{.haskell .identifier} to unfold our

-   \- seed values \`(0, {0}, makeStream numbers)\` and uses the
    `runToEnd`{.haskell .identifier} fold to

-   \- evaluate our result to get our final result. findRepeat ::
    ByteString -\> Int findRepeat = either (error . show) id . fmap
    (runToEnd . runner) . parseOnly numbers where runner :: \[Int\] -\>
    Nu (Either Int) runner ns = ana repeatCoalgebra (0, Set.singleton 0,
    makeStream \$ cata embed ns)
