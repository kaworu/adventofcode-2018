module Main (main) where

import Data.List
import Data.Map (Map, (!))
import Data.Ord (comparing)
import Data.Set (Set)
import Text.ParserCombinators.Parsec
import Text.Printf (printf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | A grid serial number.
type SerialNumber = Int

-- | A coordinate in the fuel cell grid.
type Point = (Int, Int)

-- | The power lvel in a fuel cell.
type Power = Int

-- | Grid of fuel cells.
data Grid = Grid { limit :: Int, cells, sat :: Map Point Power }
    deriving (Show)

-- | A square in the grid and the sum of its cells power.
data Square = Square { size :: Int, tl :: Point, power :: Power }
    deriving (Show)

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
-- >>> measure 8 (3, 5)
-- 4
-- >>> measure 57 (122, 79)
-- -5
-- >>> measure 39 (217, 196)
-- 0
-- >>> measure 71 (101, 153)
-- 4
measure :: SerialNumber -> Point -> Power
measure sn (x, y) = hdigit ((rid * y + sn) * rid) - 5
    where rid = rackid (x, y)

-- | The points of a nxn grid, 1-indexed.
--
-- >>> points 3
-- fromList [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]
points :: Int -> Set Point
points n = Set.fromList [(x, y) | x <- [1..n], y <- [1..n]]

-- | The Summed-area table value at (x, y). The function f map keys to values
-- from the grid, and f' map keys to values from the Summed-area table.
compute :: (Point -> Power) -> (Point -> Power) -> Point -> Power
compute f f' (x, y) = f (x1, y1) + f' (x1, y0) + f' (x0, y1) - f' (x0, y0)
    where (x0, y0) = (x - 1, y - 1)
          (x1, y1) = (x, y)

-- | The value at the given key in the map or zero.
--
-- >>> Map.fromList [('a', 1), ('b', 2)] ⦶ 'a'
-- 1
-- >>> Map.fromList [('a', 1), ('b', 2)] ⦶ 'b'
-- 2
-- >>> Map.fromList [('a', 1), ('b', 2)] ⦶ 'c'
-- 0
(⦶) :: (Ord k, Num a) => Map k a -> k -> a
(⦶) = flip (Map.findWithDefault 0)

-- | The Summed-area table of the given cells.
--
-- NOTE: the cell processing order is important and must be done from the
-- top-left to the bottom-right, hence foldl.
--
-- see https://en.wikipedia.org/wiki/Summed-area_table
img :: Map Point Power -> Map Point Power
img xs = foldl ins Map.empty (Map.keys xs)
    where ins tbl p = Map.insert p (compute (xs !) (tbl ⦶) p) tbl

-- | The nxn grid given its serial number.
grid :: Int -> SerialNumber -> Grid
grid n sn = Grid { limit = n, cells = xs, sat = img xs }
    where xs = Map.fromSet (measure sn) (points n)

-- | The square of size i having the top-left corner at (x, y) in the grid.
square :: Grid -> Int -> Point -> Square
square g i (x, y) = Square { size = i, tl = (x, y), power = area }
    where area = f' (x1, y1) + f' (x0, y0) - f' (x1, y0) - f' (x0, y1)
          (x0, y0) = (x - 1, y - 1)
          (x1, y1) = (x0 + i, y0 + i)
          f' p = sat g ⦶ p

-- | All the squares of size i from the grid.
gen :: Int -> Grid -> [Square]
gen i g = [square g i (x, y) | x <- [1..n], y <- [1..n]]
    where n = limit g - i + 1

-- | All the grid's squares.
squares :: Grid -> [Square]
squares g = concat [gen i g | i <- [1..limit g]]

-- | The square with the largest power.
--
-- >>> mightiest $ gen 3 (grid 300 18)
-- Square {size = 3, tl = (33,45), power = 29}
-- >>> mightiest $ gen 3 (grid 300 42)
-- Square {size = 3, tl = (21,61), power = 30}
mightiest :: [Square] -> Square
mightiest = maximumBy (comparing power)

-- | Display the coordinate of the top-left fuel cell of the 3x3 square with
-- the largest total power, and the X,Y,size of the square of any size with the
-- largest total power.
answer :: Square -> Square -> IO ()
answer s3 s = do
    printf "The coordinate of the top-left fuel cell of the 3x3 square with the largest total power is %d,%d,\n" s3tlx s3tly
    printf "and the X,Y,size of the square of any size with the largest total power is %d,%d,%d.\n" stlx stly (size s)
        where (s3tlx, s3tly) = tl s3
              (stlx, stly)   = tl s

-- | Compute and display the coordinate of the top-left fuel cell of the 3x3
-- square with the largest total power, and the X,Y,size of the square of any
-- size with the largest total power.
main :: IO ()
main = do
    input <- getContents
    case parse gsn "" input of
      Left err -> fail (show err)
      Right sn -> answer (mightiest $ gen 3 g) (mightiest $ squares g)
          where g = grid 300 sn

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
