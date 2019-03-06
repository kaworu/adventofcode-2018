module Main (main) where

import Data.List
import Data.Ord
import qualified Data.Map.Strict as Map
import Text.ParserCombinators.Parsec
import Text.Printf (printf)

-- | A coordinate in the fuel cell grid.
type Point = (Int, Int)

-- | The power lvel in a fuel cell.
type Power = Int

-- | Grid of fuel cells.
type Grid = Map.Map Point Power

-- | A grid serial number.
type SerialNumber = Int

-- | The rack ID of fuel cell at the given coordinate.
rackid :: Point -> Int
rackid (x, _) = x + 10

-- | The hunders digit of the given power level.
--
-- >>> hdigit 949
-- 9
-- >>> hdigit 12345
-- 3
-- >>> hdigit 99
-- 0
-- >>> hdigit 100
-- 1
hdigit :: Power -> Power
hdigit x = (x `div` 100) `mod` 10

-- | The power level of a fuel cell given its grid serial number and
-- coordinate.
--
-- >>> power 8 (3, 5)
-- 4
-- >>> power 57 (122, 79)
-- -5
-- >>> power 39 (217, 196)
-- 0
-- >>> power 71 (101, 153)
-- 4
power :: SerialNumber -> Point -> Power
power sn (x, y) = hdigit ((rid * y + sn) * rid) - 5
    where rid = rackid (x, y)

-- | The 300x300 fuel cell grid given its serial number.
grid :: SerialNumber -> Grid
grid sn = Map.fromList cells
    where cells = [((x, y), p) | x <- xs, y <- ys, let p = power sn (x, y)]
          (xs, ys) = ([1..300], [1..300])

-- | The (top-left, bottom-right) coordinates of the grid.
limits :: Grid -> (Point, Point)
limits g = ((minimum xs, minimum ys), (maximum xs, maximum ys))
    where xs = map fst pts
          ys = map snd pts
          pts = Map.keys g

-- | The top-left corner coordinate of all the 3x3 squares in the grid.
corners :: Grid -> [Point]
corners g = [(x, y) | x <- [cxmin..cxmax], y <- [cymin..cymax]]
    where (cxmin, cymin, cxmax, cymax) = (xmin, ymin, xmax - 2, ymax - 2)
          ((xmin, ymin), (xmax, ymax)) = limits g

-- | The 3x3 squares in the grid.
squares :: Grid -> [[Point]]
squares g = map extend (corners g)
    where extend (cx, cy) = [(x, y) | x <- [cx .. cx + 2], y <- [cy .. cy + 2]]

-- | The coordinate and power of the top-left fuel cell of the all 3x3 squares
-- in the grid.
reduce :: Grid -> [(Point, Power)]
reduce g = map f (squares g)
    where f xs = (head xs, sum $ map (g Map.!) xs)

-- | The coordinate and power of the top-left fuel cell of the 3x3 square with
-- the largest total power.
--
-- >>> largest (grid 18)
-- ((33,45),29)
-- >>> largest (grid 42)
-- ((21,61),30)
largest :: Grid -> (Point, Power)
largest = maximumBy (comparing snd) . reduce

-- | Display the coordinate of the top-left fuel cell of the 3x3 square with
-- the largest total power.
answer :: Point -> IO ()
answer (x, y) = printf fmt x y
    where fmt = "The coordinate of the top-left fuel cell of the 3x3 square with the largest total power is %d,%d.\n"

-- | Compute and display the coordinate of the top-left fuel cell of the 3x3
-- square with the largest total power.
main :: IO ()
main = do
    input <- getContents
    case parse gsn "" input of
      Left err -> error (show err)
      Right n  -> answer $ fst $ largest $ grid n

-- | Parse the grid serial number.
--
-- >>> parse gsn "" "57"
-- Right 57
-- >>> parse gsn "" "39"
-- Right 39
-- >>> parse gsn "" "71"
-- Right 71
gsn :: Parser SerialNumber
gsn = do
    digits <- spaces >> many1 digit
    return (read digits)
