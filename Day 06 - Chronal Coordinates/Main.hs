module Main (main) where

import Data.Char
import Data.Maybe
import Data.Function
import Data.List
import qualified Data.Map.Strict as Map
import Text.ParserCombinators.Parsec
import Text.Printf (printf)

-- | A coordinate from the network's grid.
type Point = (Int, Int)

-- | The grid's top-left corner.
zero :: Point
zero = (0, 0)

-- | The vector addition of two points.
--
-- >>> add (-1, 2) (3, -6)
-- (2,-4)
-- >>> add (1, 6) (8, 3)
-- (9,9)
add :: Point -> Point -> Point
add (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)

-- | The centroid of the given points.
--
-- see https://en.wikipedia.org/wiki/Centroid#Of_a_finite_set_of_points
--
-- >>> centroid [(-1, 2), (3, -6)]
-- (1,-2)
-- >>> centroid [(1, 1), (1, 6), (8, 3), (3, 4), (5, 5), (8, 9)]
-- (4,5)
-- >>> centroid []
-- (0,0)
centroid :: [Point] -> Point
centroid xs = (avg mx, avg my)
    where avg x = round $ realToFrac x / (genericLength xs :: Double)
          (mx, my) = foldl' add zero xs

-- | The bottom-right point of the square containing all the given points.
--
-- >>> limit [(1, 1), (1, 6), (8, 3), (3, 4), (5, 5), (8, 9)]
-- (8,9)
-- >>> limit [(1, 1), (1, 6), (8, 3), (3, 4), (5, 5)]
-- (8,6)
-- >>> limit []
-- (0,0)
limit :: [Point] -> Point
limit [] = zero
limit xs = (max' fst, max' snd)
    where max' f = maximum $ map f xs

-- | Sized area of closest locations, either finite or infinite.
data Area = Infinite | Finite Int
    deriving (Show)

-- | Just the area size if it is finite, Nothing otherwise.
--
-- >>> size Infinite
-- Nothing
-- >>> size (Finite 42)
-- Just 42
size :: Area -> Maybe Int
size Infinite   = Nothing
size (Finite x) = Just x

-- | Increase the area size by one.
--
-- >>> expand (Finite 41)
-- Finite 42
-- >>> expand Infinite
-- Infinite
expand :: Area -> Area
expand Infinite   = Infinite
expand (Finite x) = Finite (x + 1)

-- | Manhattan distance from a Point relative to another.
type Distance = Int

-- | Manhattan's distance between two points.
--
-- >>> distance (4, 3) (1, 1)
-- 5
-- >>> distance (4, 3) (1, 6)
-- 6
-- >>> distance (4, 3) (8, 3)
-- 4
-- >>> distance (4, 3) (3, 4)
-- 2
-- >>> distance (4, 3) (5, 5)
-- 3
-- >>> distance (4, 3) (8, 9)
-- 10
distance :: Point -> Point -> Distance
distance (x0, y0) (x1, y1) = abs (x0 - x1) + abs (y0 - y1)

-- | Identifier for the listed coordinates.
type Color = Int

-- | A location in the grid is defined by its distance with respect to it
-- closest color(s). It is tied if it has more than one color. A listed
-- coordinate's location has a distance of 0.
data Location = Location Distance [Color]
    deriving (Show)

-- | The location network is defined by its width, height, and its grid.
data Net = Net Int Int (Map.Map Point Location)

-- | Show a network kind of like the README present it.
-- We use comma (,) for unknown location instead of dot (.) because dot mean
-- tied.
instance Show Net where
    show net@(Net w h _) = intercalate "\n" (map glyphs rows)
        where rows = groupBy ((==) `on` snd) pts
              pts = [(x, y) | y <- [0..h], x <- [0 ..w]]
              glyphs = map $ glyph . locate net
              glyph Nothing = ',' -- unknown
              glyph (Just (Location 0 [c])) = chr (ord 'A' + c)
              glyph (Just (Location _ [c])) = chr (ord 'a' + c)
              glyph (Just (Location _ _))   = '.' -- tied

-- | An empty network given its limiting bottom-right Point.
blank :: Point -> Net
blank (w, h) = Net w h Map.empty

-- | The location in the network at the given Point, if any.
locate :: Net -> Point -> Maybe Location
locate (Net _ _ g) (x, y) = Map.lookup (x, y) g

-- | Replace or create the location in the network at the given Point.
update :: Net -> Point -> Location -> Net
update (Net w h g) (x, y) l = Net w h $ Map.insert (x, y) l g

-- | Compute each network's color area size (both finite and infinite).
areas :: Net -> Map.Map Color Area
areas (Net w h g) = Map.foldrWithKey mark Map.empty g
    where mark (x, y) (Location _ [c]) acc
            | border (x, y) = Map.insert c Infinite acc
            | otherwise     = Map.alter increase c acc
          mark _ _ acc = acc -- tied
          border (x, y) = x == 0 || x == w || y == 0 || y == h
          increase = Just . maybe (Finite 1) expand

-- | A flood fill request to paint a color at a position.
data Brush = Brush { position :: Point, color :: Color }
    deriving (Show)

-- | Flood fill starting from an initial list of color coordinates.
--
-- Keep track of the distance of flooding from the color start point, so that
-- we stop flooding once we reach a location that is colored with a color that
-- is closest. Thus, the flooding process is BFS in the sense that each step
-- flood each color for a given distance.
--
-- The `out` expression is True when flooding should ignore the given point in
-- the net and stop flooding, False otherwise.
--
-- see https://en.wikipedia.org/wiki/Flood_fill
fill :: Net -> (Point -> Bool) -> Distance -> [Brush] -> [Brush] -> Net
fill net _ _ [] [] = net
fill net out d ns [] = fill net out (d + 1) [] ns
fill net out d ns (Brush (x, y) c : bs) =
    case locate net (x, y) of
      Nothing | out (x, y)               -> ignore
      Nothing                            -> paint
      Just (Location d' _) | d < d'      -> paint
      Just (Location d' _) | d > d'      -> ignore
      Just (Location _ cs) | c `elem` cs -> ignore
      Just (Location _ cs)               -> tie (c : cs)
    where ignore = fill net out d ns bs
          paint  = fill (update net (x, y) (Location d [c])) out d flood bs
          tie cs = fill (update net (x, y) (Location d cs))  out d flood bs
          flood  = north c : east c : south c : west c : ns
              where north  = Brush (x, y - 1)
                    east   = Brush (x + 1, y)
                    south  = Brush (x, y + 1)
                    west   = Brush (x - 1, y)

-- | The net having an area per given coordinate containing all the locations
-- that are the closest.
--
-- Create a Brush assigning a color per point in the listed coordinates and
-- flood-fill a blank net with them.
closest :: [Point] -> Net
closest xs = fill (blank (w, h)) out 0 [] bs
    where (w, h) = limit xs
          out (x, y) = x < 0 || x > w || y < 0 || y > h
          bs = zipWith Brush xs [0..]

-- | The net with a single area containing all locations having a total
-- distance to the given coordinates less than the given maximum distance.
--
-- Create a single Brush starting from the centroid point (with respect to the
-- listed coordinates) and flood-fill a blank net with it. Any location that is
-- too far is considered out.
central :: Distance -> [Point] -> Net
central d xs = fill (blank (w, h)) (\p -> out p || unsafe p) 0 [] [center]
    where (w, h) = limit xs
          out (x, y) = x < 0 || x > w || y < 0 || y > h
          unsafe (x, y) = sum (map (distance (x, y)) xs) >= d
          center = Brush (centroid xs) 0

-- | Display the size of the largest area that isn't infinite, and the count of
-- locations having a total distance to all the listed coordinates less than
-- the limit.
answer :: Int -> Int -> Distance -> IO ()
answer u s d = do
    printf "The size of largest area that isn't infinite is %d,\n" u
    printf "and there are %d locations which have a total distance " s
    printf "to all given coordinates of less than %d.\n" d

-- | Compute and display the size of the largest area that isn't infinite, and
-- the count of locations having a total distance to all the listed coordinates
-- less than the limit.
main :: IO ()
main = do
    input <- getContents
    case parse points "" input of
      Left err -> error (show err)
      Right xs -> answer (largest dangerous) (largest safe) lim
          where largest   = maximum . mapMaybe size . Map.elems . areas
                dangerous = closest xs
                safe      = central lim xs
                lim       = 10000

-- | Parse a coordinate.
--
-- >>> parse point "" "1, 6"
-- Right (1,6)
point :: Parser Point
point = do
    x <- many1 digit
    y <- char ',' >> spaces >> many1 digit
    spaces
    return (read x, read y)

-- | Parse a sequence of coordinate.
--
-- >>> parse points "" "1, 1\n1, 6"
-- Right [(1,1),(1,6)]
points :: Parser [Point]
points = many1 point <* eof
