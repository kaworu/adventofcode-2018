module Main (main) where

import Data.IntMap (IntMap, (!))
import Text.ParserCombinators.Parsec
import Text.Printf (printf)
import qualified Data.IntMap.Strict as IntMap

-- A recipe score.
type Score = Int

-- | The kitchen's scoreboard for all cooked recipes.
type ScoreBoard = IntMap Score

-- | The current recipe of both elves.
type Elves = (Int, Int)

-- | The whole laboratory.
--
-- Keeping track of the length is important for performance reasons as
-- IntMap.size is O(n).
data Kitchen = Kitchen { elves :: Elves, len :: Int, scores :: ScoreBoard }
    deriving (Show)

-- | The initial scoreboard.
--
-- >>> board
-- Kitchen {elves = (0,1), len = 2, scores = fromList [(0,3),(1,7)]}
board :: Kitchen
board = Kitchen (0, 1) 2 $ IntMap.fromList [(0, 3), (1, 7)]

-- | Add the recipe to the kitchen's scoreboard.
--
-- >>> append 10 board
-- Kitchen {elves = (0,1), len = 4, scores = fromList [(0,3),(1,7),(2,1),(3,0)]}
append :: Score -> Kitchen -> Kitchen
append x k@Kitchen { len = l, scores = s }
  | x >= 10 = append (x `mod` 10) $ append (x `div` 10) k
  | otherwise = Kitchen (elves k) (l + 1) (IntMap.insert l x s)

-- | Cook a round of recipe.
--
-- >>> cook board
-- Kitchen {elves = (0,1), len = 4, scores = fromList [(0,3),(1,7),(2,1),(3,0)]}
-- >>> cook $ cook board
-- Kitchen {elves = (4,3), len = 6, scores = fromList [(0,3),(1,7),(2,1),(3,0),(4,1),(5,0)]}
-- >>> iterate cook board !! 3
-- Kitchen {elves = (6,4), len = 7, scores = fromList [(0,3),(1,7),(2,1),(3,0),(4,1),(5,0),(6,1)]}
cook :: Kitchen -> Kitchen
cook k@Kitchen { elves = (a, b), scores = s } = Kitchen (a', b') l' s'
    where a' = (a + sa + 1) `mod` l'
          b' = (b + sb + 1) `mod` l'
          Kitchen { len = l', scores = s' } = append (sa + sb) k
          (sa, sb) = (s ! a, s ! b)

-- | The scores of the i recipes after n recipes.
--
-- >>> ultimate 10 5 board
-- [0,1,2,4,5,1,5,8,9,1]
-- >>> ultimate 10 18 board
-- [9,2,5,1,0,7,1,0,8,5]
-- >>> ultimate 10 2018 board
-- [5,9,4,1,4,2,9,8,8,2]
ultimate :: Int -> Int -> Kitchen -> [Int]
ultimate i n k@Kitchen { len = l, scores = s }
  | l >= (n + i) = take i $ drop n $ IntMap.elems s
  | otherwise    = ultimate i n $ cook k

-- | Display the 10 recipes immediately after a given number of recipes.
answer :: Int -> [Score] -> IO ()
answer n xs = printf fmt n (concat $ map show xs)
    where fmt = "the scores of the ten recipes immediately after %d are %s.\n"

-- | Compute and display the 10 recipes immediately after a given number of
-- recipes.
main :: IO ()
main = do
    input <- getContents
    case parse number "" input of
      Left err -> fail (show err)
      Right n  -> answer n (ultimate 10 n board)

-- | Parse the recipes count.
--
-- >>> parse number "" "57"
-- Right 57
-- >>> parse number "" "39"
-- Right 39
-- >>> parse number "" "71"
-- Right 71
number :: Parser Int
number = do
    digits <- spaces >> many1 digit
    return (read digits)
