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
-- >>> freq [1, 2, 2, 3, 3, 3]
-- [(1,1),(2,2),(3,3)]
freq :: Ord a => [a] -> [(a, Int)]
freq = map (\xs -> (head xs, length xs)) . group . sort

-- | Display the box IDs list checksum.
answer :: Int -> IO ()
answer = printf "The checksum is %d.\n"

-- | Compute and display the rudimentary checksum of the given list of box IDs.
main :: IO ()
main = do
    input <- getContents
    case parse boxIds "" input of
      Left err -> print err >> fail "parse error"
      Right xs -> answer ((length twos) * (length threes))
          where twos   = filter (any (==2)) freqs
                threes = filter (any (==3)) freqs
                freqs  = map (map snd . freq) xs

-- | Parse a BoxID.
--
-- >>> parse boxId "" "foobar"
-- Right "foobar"
boxId :: Parser BoxID
boxId = do
    id <- many1 lower
    _  <- spaces
    return id

-- | Parse a sequence of BoxID.
--
-- >>> parse boxIds "" "abcdef bababc abbcde"
-- Right ["abcdef","bababc","abbcde"]
boxIds :: Parser [BoxID]
boxIds = (sepBy1 boxId spaces) <* eof
