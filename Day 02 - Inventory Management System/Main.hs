module Main (main) where

import Data.List
import Text.Printf (printf)

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

-- | A box ID
type BoxID = [Char]

-- | The frequency of each elements within a list.
--
-- >>> freq "abcdef"
-- [('a',1),('b',1),('c',1),('d',1),('e',1),('f',1)]
-- >>> freq "bababc"
-- [('a',2),('b',3),('c',1)]
-- >>> freq "abbcde"
-- [('a',1),('b',2),('c',1),('d',1),('e',1)]
-- >>> freq "abcccd"
-- [('a',1),('b',1),('c',3),('d',1)]
-- >>> freq "aabcdd"
-- [('a',2),('b',1),('c',1),('d',2)]
-- >>> freq "abcdee"
-- [('a',1),('b',1),('c',1),('d',1),('e',2)]
-- >>> freq "ababab"
-- [('a',3),('b',3)]
-- >>> freq []
-- []
freq :: Ord a => [a] -> [(a, Int)]
freq = map (\xs -> (head xs, length xs)) . group . sort

-- | The box IDs list checksum.
--
-- >>> checksum ["abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab"]
-- 12
-- >>> checksum []
-- 0
checksum :: [BoxID] -> Int
checksum xs = (length twos) * (length threes)
    where twos   = filter (any (==2)) freqs
          threes = filter (any (==3)) freqs
          freqs  = map (map snd . freq) xs

-- | The hamming distance of two lists.
-- | see https://stackoverflow.com/a/52277553.
--
-- >>> distance "abcde" "axcye"
-- 2
-- >>> distance "fghij" "fguij"
-- 1
distance :: Eq a => [a] -> [a] -> Int
distance xs ys = length $ filter id $ zipWith (/=) xs ys

-- | All uniques pairs of a list.
-- | see https://stackoverflow.com/a/34045121
--
-- >>> pairs [1, 2, 3, 4]
-- [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
-- >>> pairs []
-- []
-- >>> pairs ["alone"]
-- []
-- >>> pairs ["fst", "snd"]
-- [("fst","snd")]
pairs :: [a] -> [(a, a)]
pairs l = [(x, y) | (x:ys) <- tails l, y <- ys]

-- | The pair of BoxID which differ by exactly one character at the same
-- position.
--
-- >>> prototypes ["abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz"]
-- Just ("fghij","fguij")
-- >>> prototypes ["abcde", "axcye"]
-- Nothing
-- >>> prototypes []
-- Nothing
prototypes :: [BoxID] -> Maybe (BoxID, BoxID)
prototypes = find matchingDistance . pairs
    where matchingDistance (x, y) = (distance x y) == 1

-- | Display the box IDs list checksum and the common letters between the two
-- correct box IDs.
answer :: Int -> Maybe [Char] -> IO ()
answer i Nothing   = printf "The checksum is %d, there are no correct box IDs.\n" i
answer i (Just xs) = printf "The checksum is %d, \"%s\" are the common letters between the two correct box IDs.\n" i xs

-- | Compute and display the rudimentary checksum of the given list of box IDs.
main :: IO ()
main = do
    input <- getContents
    case parse boxIds "" input of
      Left err -> print err >> fail "parse error"
      Right xs -> answer (checksum xs) (common $ prototypes xs)
          where common Nothing       = Nothing
                common (Just (a, b)) = Just (a `intersect` b)

-- | Parse a BoxID.
--
-- >>> parse boxId "" "foobar"
-- Right "foobar"
boxId :: Parser BoxID
boxId = do
    s <- many1 lower
    _ <- spaces
    return s

-- | Parse a sequence of BoxID.
--
-- >>> parse boxIds "" "abcdef bababc abbcde"
-- Right ["abcdef","bababc","abbcde"]
boxIds :: Parser [BoxID]
boxIds = (sepBy1 boxId spaces) <* eof
