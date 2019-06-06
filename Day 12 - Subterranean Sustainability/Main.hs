module Main (main) where

import Data.Map (Map, (!))
import Text.ParserCombinators.Parsec
import Text.Printf (printf)
import qualified Data.Map.Strict as Map

-- | A pot that either contain a plant or doesn't.
data Pot = Empty | Plant
    deriving (Eq, Ord)

-- | The pattern part of a note.
data Pattern = Pattern Pot Pot Pot Pot Pot
    deriving (Eq, Ord)

-- | The notes found on the nearby table.
type Notes = Map Pattern Pot

-- | The tunnel with its generation, first pot number, pots, and notes.
data Garden = Garden { num :: Int, pots :: [Pot], notes :: Notes }
    deriving (Show)

-- | Display a pot as in README.md
--
-- >>> show Empty
-- "."
-- >>> show Plant
-- "#"
instance Show Pot where
    show Empty = "."
    show Plant = "#"

-- | Display a note pattern as in README.md
--
-- >>> show $ Pattern Empty Plant Empty Plant Empty
-- ".#.#."
instance Show Pattern where
    show (Pattern ll l c r rr) = concatMap show [ll, l, c, r, rr]

-- | The given pots without the leading Empty ones.
--
-- >>> lstrip [Empty, Plant, Empty]
-- [#,.]
-- >>> lstrip [Plant, Empty, Empty]
-- [#,.,.]
-- >>> lstrip [Empty, Empty, Empty]
-- []
-- >>> lstrip []
-- []
lstrip :: [Pot] -> [Pot]
lstrip (Empty : xs) = lstrip xs
lstrip xs = xs

-- | The given pots without the trailing Empty ones.
--
-- >>> rstrip [Empty, Plant, Empty]
-- [.,#]
-- >>> rstrip [Plant, Empty, Empty]
-- [#]
-- >>> rstrip [Empty, Empty, Empty]
-- []
-- >>> rstrip []
-- []
rstrip :: [Pot] -> [Pot]
rstrip = reverse . lstrip . reverse

-- | An incomplete Pattern missing only the rightmost pot, build from the
-- previous incomplete pattern and the pot two spots to the right of the
-- current one.
shiftl :: (Pot -> Pattern) -> Pot -> (Pot -> Pattern)
shiftl f p = Pattern l c r rr
    where (Pattern _ l c r rr) = f p

-- | The same Garden without the leading and trailing empty pots accounted for
-- in its state.
simplify :: Garden -> Garden
simplify (Garden n ps m) = Garden (n + nlstripped) stripped m
    where nlstripped = length ps - length lstripped
          lstripped = lstrip ps
          stripped = rstrip lstripped

-- | The next generation of the given garden.
next :: Garden -> Garden
next (Garden n ps m) = simplify $ Garden (n - 2) (evo p0 ps) m
    where p0 = Pattern Empty Empty Empty Empty
          evo f (x : xs) = (m ! f x) : evo (shiftl f x) xs
          evo f [] = fin f (replicate 4 Empty)
          fin f (x : xs) = (m ! f x) : fin (shiftl f x) xs
          fin _ [] = []

-- | The given garden after i generation.
grow :: Int -> Garden -> Garden
grow 0 g = g
grow i g = grow' (i - 1) g (next g)

-- | Memoized version of grow detecting when the state stabilize.
--
-- This implementation has the limitation of short-cutting only when the state
-- stabilize by shifting itself. We could detect loops by using a Map,
-- effectively memoizing all the state already seen, but remembering only the
-- last state is enough for this challenge.
grow' :: Int -> Garden -> Garden -> Garden
grow' 0 _ g = g
grow' i prev g@(Garden n ps m)
  | pots prev == ps = Garden (n + i * (n - num prev)) ps m
  | otherwise = grow' (i - 1) g (next g)

-- | The sum of the number of all pots which contain a plant.
plants :: Garden -> Int
plants (Garden n ps _) = sum [ x | (Plant, x) <- zip ps [n..] ]

-- | Display the sum of the numbers of all pots which contain a plant after 20
-- generations.
answer :: Garden -> Garden -> IO ()
answer g20 g50b = do
    printf "The sum of the numbers of all pots which contain a plant after 20 generations is %d,\n" (plants g20)
    printf "and after fifty billion generations it is %d.\n" (plants g50b)

-- | Compute and display the sum of the numbers of all pots which contain a
-- plant after 20 generations.
main :: IO ()
main = do
    input <- getContents
    case parse garden "" input of
      Left err -> fail (show err)
      Right g0 -> answer g20 g50b
          where g20  = grow 20 g0
                g50b = grow (50000000000 - 20) g20

-- | Parse '.' as an Empty pot and '#' as a Plant.
--
-- >>> parse pot "" "."
-- Right .
-- >>> parse pot "" "#"
-- Right #
-- >>> parse pot "" "x"
-- Left (line 1, column 1):
-- unexpected "x"
pot :: Parser Pot
pot = do
    c <- oneOf ".#"
    return (if c == '.' then Empty else Plant)

-- | Parse the garden initial state
--
-- >>> parse initialState "" "initial state: #..#.#..##......###...###"
-- Right [#,.,.,#,.,#,.,.,#,#,.,.,.,.,.,.,#,#,#,.,.,.,#,#,#]
initialState :: Parser [Pot]
initialState = spaces >> string "initial state: " >> many1 pot

-- | Parse a note found on the nearby table.
--
-- >>> parse note "" "...## => #"
-- Right (...##,#)
-- >>> parse note "" "..#.. => #"
-- Right (..#..,#)
-- >>> parse note "" ".#... => #"
-- Right (.#...,#)
note :: Parser (Pattern, Pot)
note = do
     _  <- spaces
     ll <- pot
     l  <- pot
     c  <- pot
     r  <- pot
     rr <- pot
     n  <- spaces >> string "=>" >> spaces >> pot <* spaces
     return (Pattern ll l c r rr, n)

-- | Parse the garden state and the notes.
garden :: Parser Garden
garden = do
    st <- initialState
    xs <- many1 note
    return $ Garden { num = 0, pots = st, notes = Map.fromList xs }
