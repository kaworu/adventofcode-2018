module Main (main) where

import Data.Char
import Data.Maybe
import Data.Function
import Data.List
import qualified Data.Map.Strict as Map
import Text.ParserCombinators.Parsec
import Text.Printf (printf)

-- | A coordinate from the network's grid.
-- NOTE: By convention (0, 0) is the top-left corner.
type Point = (Int, Int)

-- | The bottom-right Point of the square starting from (0, 0) containing all
-- the given points.
--
-- >>> limit [(1, 1), (1, 6), (8, 3), (3, 4), (5, 5), (8, 9)]
-- (8,9)
-- >>> limit [(1, 1), (1, 6), (8, 3), (3, 4), (5, 5)]
-- (8,6)
-- >>> limit []
-- (0,0)
limit :: [Point] -> Point
limit [] = (0, 0)
limit xs = (maximum $ map fst xs, maximum $ map snd xs)

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

-- | Identifier for the listed coordinates.
type Color = Int

-- | A location in the grid is defined by its distance with respect to it
-- closest color(s). It is tied if it has more than one color. A listed
-- coordinate's location has a distance of 0.
data Location = Location { distance :: Distance, colors :: [Color] }
    deriving (Show)

-- | The location network is defined by its size and its grid.
data Net = Net { width, height :: Int, grid :: Map.Map Point Location }

-- | Show a network kind of like the README present it.
-- We use comma (,) for unknown location instead of dot (.) because dot mean
-- tied.
instance Show Net where
    show net = intercalate "\n" (map glyphs rows)
        where rows = groupBy ((==) `on` snd) (points net)
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
locate net (x, y) = Map.lookup (x, y) (grid net)

-- | Replace or create the location in the network at the given Point.
update :: Net -> Point -> Location -> Net
update (Net w h g) (x, y) l = Net w h $ Map.insert (x, y) l g

-- | All the network's points.
points :: Net -> [Point]
points (Net w h _) = [(x, y) | y <- [0..h], x <- [0..w]]

-- | Compute each network's color area size (both finite and infinite).
areas :: Net -> Map.Map Color Area
areas (Net w h g) = Map.foldrWithKey mark Map.empty g
    where mark (x, y) (Location _ [c]) acc
            | border (x, y) = Map.insert c Infinite acc
            | otherwise     = Map.alter increase c acc
          mark _ _ acc = acc -- tied
          border (x, y) = x == 0 || x == w || y == 0 || y == h
          increase = Just . maybe (Finite 1) expand

-- | A flood fill step painting a color at a position.
data Brush = Brush { position :: Point, color :: Color }
    deriving (Show)

-- | Given the initial list of coordinates, compute the closest(s) color at
-- each location in the net.
fill :: [Brush] -> Net
fill xs = fill' (blank $ limit $ map position xs) 0 [] xs

-- | Flood fill starting from the initial list of coordinates keeping track of
-- the current distance.
-- see https://en.wikipedia.org/wiki/Flood_fill
fill' :: Net -> Distance -> [Brush] -> [Brush] -> Net
fill' net _ [] [] = net
fill' net d ns [] = fill' net (d + 1) [] ns
fill' net d ns (Brush (x, y) c : bs) =
    case locate net (x, y) of
      Nothing                            -> if out then ignore else paint
      Just (Location d' _) | d < d'      -> paint
      Just (Location d' _) | d > d'      -> ignore
      Just (Location _ cs) | c `elem` cs -> ignore
      Just (Location _ cs)               -> tie (c : cs)
    where out = x < 0 || x > width net || y < 0 || y > height net
          ignore = fill' net d ns bs
          paint  = fill' (update net (x, y) (Location d [c])) d flood bs
          tie cs = fill' (update net (x, y) (Location d cs))  d flood bs
          flood  = north c : east c : south c : west c : ns
              where north  = Brush (x, y - 1)
                    east   = Brush (x + 1, y)
                    south  = Brush (x, y + 1)
                    west   = Brush (x - 1, y)

-- | Display the size of the largest area that isn't infinite.
answer :: Int -> IO ()
answer = printf "the size of largest area that isn't infinite is %d.\n"

-- | Compute and display the size of the largest area that isn't infinite.
main :: IO ()
main = do
    input <- getContents
    case parse brushes "" input of
      Left err -> error (show err)
      Right xs ->
          answer $ maximum $ mapMaybe size $ Map.elems $ areas (fill xs)

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

-- | Parse a sequence of coordinate and assign them colors (in order).
--
-- >>> parse brushes "" "1, 1\n1, 6"
-- Right [Brush (1,1) 0,Brush (1,6) 1]
brushes :: Parser [Brush]
brushes = do
    pts <- sepBy1 point spaces
    eof
    return $ zipWith Brush pts [0..]
