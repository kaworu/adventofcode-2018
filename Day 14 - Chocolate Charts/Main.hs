module Main (main) where

import Data.Char
import Data.List
import Text.ParserCombinators.Parsec
import Text.Printf (printf)
import qualified Data.Sequence as Seq

-- A recipe score.
type Score = Int

-- | The scoreboard.
--
-- >>> take 20 board
-- [3,7,1,0,1,0,1,2,4,5,1,5,8,9,1,6,7,7,9,2]
board :: [Score]
board = 3 : 7 : next (0, 1) (Seq.fromList [3, 7]) []
    where next (a, b) full (x : xs) = x : next (a, b) full xs
          next (a, b) full [] = next (a', b') full' new
            where new = if s < 10 then [s] else [s `div` 10, s `mod` 10]
                  s = sa + sb
                  sa = Seq.index full a
                  sb = Seq.index full b
                  full' = full <> Seq.fromList new
                  a' = (a + sa + 1) `mod` length full'
                  b' = (b + sb + 1) `mod` length full'

-- | The scores of the i recipes after n recipes.
--
-- >>> iafter 10 5
-- [0,1,2,4,5,1,5,8,9,1]
-- >>> iafter 10 18
-- [9,2,5,1,0,7,1,0,8,5]
-- >>> iafter 10 2018
-- [5,9,4,1,4,2,9,8,8,2]
iafter :: Int -> Int -> [Score]
iafter i n = take i $ drop n board

-- | The number of recipes after which the given score pattern appears.
--
-- >>> ultimate [5, 1, 5, 8, 9]
-- 9
-- >>> ultimate [0, 1, 2, 4, 5]
-- 5
-- >>> ultimate [9, 2, 5, 1, 0]
-- 18
-- >>> ultimate [5, 9, 4, 1, 4]
-- 2018
ultimate :: [Int] -> Int
ultimate xs = length $ takeWhile (not . (xs `isPrefixOf`)) $ tails board

-- | Display the 10 recipes immediately after a given number of recipes, and
-- the number of recipes after which the score pattern appears.
answer :: String -> [Score] -> Int -> IO ()
answer desc xs n = do
    printf "the scores of the ten recipes immediately after %d recipes are %s,\n" (read desc :: Int) (concat $ map show xs)
    printf "and %s first appears after %d recipes.\n" desc n

-- | Compute and display the 10 recipes immediately after a given number of
-- recipes, and the number of recipes after which the score pattern appears.
main :: IO ()
main = do
    input <- getContents
    case parse digits "" input of
      Left err -> fail (show err)
      Right desc -> answer desc (iafter 10 n) (ultimate $ map digitToInt desc)
          where n = read desc

-- | Parse the recipes count.
--
-- >>> parse digits "" "57"
-- Right "57"
-- >>> parse digits "" "039"
-- Right "039"
-- >>> parse digits "" "71"
-- Right "71"
-- >>> parse digits "" "NOPE"
-- Left (line 1, column 1):
-- unexpected "N"
-- expecting white space or digit
digits :: Parser String
digits = spaces >> many1 digit
