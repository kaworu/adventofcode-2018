module Main (main) where

import Data.Map (Map, (!))
import Text.ParserCombinators.Parsec
import Text.Printf (printf)
import qualified Data.Map.Strict as Map

-- | A pot that either contain a plant or doesn't.
data Pot = Empty | Plant
    deriving (Eq, Ord)

-- | The pattern part of a note found on a nearby table.
data Pattern = Pattern Pot Pot Pot Pot Pot
    deriving (Eq, Ord)

-- | The tunnel with the first pot's number, the pots, and the notes.
data Tunnel = Tunnel Int [Pot] (Map Pattern Pot)

-- | Display a pot as in README.md
--
-- >> show Empty
-- "."
-- >> show Plant
-- "#"
instance Show Pot where
    show Empty = "."
    show Plant = "#"

-- | Display a note pattern as in README.md
--
-- >> show $ Pattern Empty Pot Empty Pot Empty
-- ".#.#."
instance Show Pattern where
    show (Pattern ll l c r rr) = concatMap show [ll, l, c, r, rr]

-- | Display the first pot number and the state of the tunnel.
instance Show Tunnel where
    show (Tunnel i pots _) = printf "%d\n%s" i (concatMap show pots)

-- | An incomplete Pattern missing only the rightmost pot, build from the
-- previous incomplete pattern and the pot two spots to the right of the
-- current one.
shiftl :: (Pot -> Pattern) -> Pot -> (Pot -> Pattern)
shiftl f p = Pattern l c r rr
    where (Pattern _ l c r rr) = f p

-- | The same Tunnel without the leading and trailing empty pots accounted for.
simplify :: Tunnel -> Tunnel
simplify (Tunnel i (Empty : xs) m) = simplify $ Tunnel (i + 1) xs m
simplify (Tunnel i pots m) = Tunnel i (reverse $ chomp rev) m
    where rev = reverse pots
          chomp (Empty : xs) = xs
          chomp xs = xs

-- | The next generation of the given tunnel.
next :: Tunnel -> Tunnel
next (Tunnel i pots m) = simplify $ Tunnel (i - 2) (ev p0 pots) m
    where p0 = Pattern Empty Empty Empty Empty
          ev f (x : xs) = (m ! f x) : ev (shiftl f x) xs
          ev f [] = fin f (replicate 4 Empty)
          fin f (x : xs) = (m ! f x) : fin (shiftl f x) xs
          fin _ [] = []

-- | The sum of the number of all pots which contain a plant.
plants :: Tunnel -> Int
plants (Tunnel i pots _) = sum $ map snd $ filter planted $ zip pots [i..]
    where planted (p, _) = p == Plant

-- | Display the sum of the numbers of all pots which contain a plant after 20
-- generations.
answer :: Tunnel -> IO ()
answer = printf "The sum of the numbers of all pots which contain a plant after 20 generations is %d.\n" . plants

-- | Compute and display the sum of the numbers of all pots which contain a
-- plant after 20 generations.
main :: IO ()
main = do
    input <- getContents
    case parse tunnel "" input of
      Left err -> error (show err)
      Right  t -> answer $ last $ take 21 $ iterate next t

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

-- | Parse the tunnel's initial state
--
-- >>> parse state "" "initial state: #..#.#..##......###...###"
-- Right [#,.,.,#,.,#,.,.,#,#,.,.,.,.,.,.,#,#,#,.,.,.,#,#,#]
state :: Parser [Pot]
state = spaces >> string "initial state: " >> many1 pot

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

-- | Parse the tunnel's state and the notes.
tunnel :: Parser Tunnel
tunnel = do
    s  <- state
    xs <- many1 note
    return $ Tunnel 0 s (Map.fromList xs)
