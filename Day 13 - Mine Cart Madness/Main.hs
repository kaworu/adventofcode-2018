module Main (main) where

import Data.Either
import Data.List
import Data.Map.Strict (Map, (!))
import Data.Maybe
import Data.Ord
import Text.ParserCombinators.Parsec
import Text.Printf (printf)
import qualified Data.Map.Strict as Map

-- | A (x, y) coordinate in the crop.
type Point = (Int, Int)

-- | A cart direction in the crop.
data Direction = North | East | South | West
    deriving (Eq)

-- | A track on which carts may drive.
data Track = Pipe | Dash | Slash | BackSlash | Cross
    deriving (Eq)

-- | All the tracks in the crop.
type Tracks = Map Point Track

-- | The options that a cart has to turn, in order.
data Turning = Leftwise | Straight | Rightwise
    deriving (Eq, Show)

-- | A cart in the crop.
data Cart = Cart { pos :: Point, facing :: Direction, turning :: Turning }
    deriving (Eq, Show)

-- | Display a cart direction in the same fashion as it is done in the README.
instance Show Direction where
    show North = "^"
    show East  = ">"
    show South = "v"
    show West  = "<"

-- | Display a track in the same fashion as it is done in the README.
instance Show Track where
    show Pipe      = "|"
    show Dash      = "-"
    show Slash     = "/"
    show BackSlash = "\\"
    show Cross     = "+"

-- | True if the two given carts have the same position, False otherwise.
--
-- >>> samePos (Cart (1, 0) North Leftwise) (Cart (0, 0) North Leftwise)
-- False
-- >>> samePos (Cart (0, 0) North Leftwise) (Cart (0, 1) North Leftwise)
-- False
-- >>> samePos (Cart (0, 0) East Straight) (Cart (0, 0) South Rightwise)
-- True
samePos :: Cart -> Cart -> Bool
samePos c w = pos c == pos w

-- | Just the first cart in from the list having crashed with the given cart,
-- or Nothing.
--
-- >>> crash (Cart (0, 0) North Leftwise) $ []
-- Nothing
-- >>> crash (Cart (0, 0) North Leftwise) $ [Cart (0, -1) North Leftwise]
-- Nothing
-- >>> crash (Cart (0, 0) North Leftwise) $ [Cart (0, -1) North Leftwise, Cart (0, 0) South Straight]
-- Just (Cart {pos = (0,0), facing = v, turning = Straight})
crash :: Cart -> [Cart] -> Maybe Cart
crash c = find (samePos c)

-- | The cart's direction at a given track.
heading :: Track -> Turning -> Direction -> Direction
heading Pipe _ d = d
heading Dash _ d = d
heading Slash _ North = East
heading Slash _ South = West
heading Slash _ West  = South
heading Slash _ East  = North
heading BackSlash _ North = West
heading BackSlash _ South = East
heading BackSlash _ West  = North
heading BackSlash _ East  = South
heading Cross Straight d = d
heading Cross Rightwise North = East
heading Cross Rightwise East  = South
heading Cross Rightwise South = West
heading Cross Rightwise West  = North
heading Cross Leftwise North = West
heading Cross Leftwise West  = South
heading Cross Leftwise South = East
heading Cross Leftwise East  = North

-- | The next cart's position.
--
-- >>> move (0, 0) North
-- (0,-1)
-- >>> move (1, 1) South
-- (1,2)
-- >>> move (1, -1) West
-- (0,-1)
-- >>> move (41, 9001) East
-- (42,9001)
move :: Point -> Direction -> Point
move (x, y) North = (x, y - 1)
move (x, y) South = (x, y + 1)
move (x, y) West  = (x - 1, y)
move (x, y) East  = (x + 1, y)

-- | The next cart's turning orientation.
swing :: Track -> Turning -> Turning
swing Cross Leftwise  = Straight
swing Cross Straight  = Rightwise
swing Cross Rightwise = Leftwise
swing _ u = u

-- | The cart at the next step.
step :: Tracks -> Cart -> Cart
step ts (Cart p d u) = Cart p' d' u'
    where d' = heading t u d
          p' = p `move` d'
          u' = swing t u
          t  = ts ! p

-- | Tick until all crash happened.
tick' :: Tracks -> [Point] -> [Cart] -> [Cart] -> ([Point], Maybe Point)
tick' _ crashed []  [] = (reverse crashed, Nothing)
tick' _ crashed [c] [] = (reverse crashed, Just $ pos c)
tick' ts crashed moved [] = tick' ts crashed [] $ sortBy (comparing pos) moved
tick' ts crashed moved (c : cs) =
    let c' = step ts c
     in case crash c' (moved ++ cs) of
          Nothing -> tick' ts crashed (c' : moved) cs
          Just w  -> tick' ts (pos c' : crashed) (delete w moved) (delete w cs)

-- | Tick until all crash happened.
drive :: Tracks -> [Cart] -> ([Point], Maybe Point)
drive ts cs = tick' ts [] cs []

-- | Display the location of the first crash and the last car's location.
answer :: Point -> Maybe Point -> IO ()
answer (x, y) (Just (x', y')) = do
    printf "The location of the first crash is %d,%d,\n" x y
    printf "and the location of the last cart is %d,%d.\n" x' y'
answer (x, y) Nothing = do
    printf "The location of the first crash is %d,%d,\n" x y
    printf "and there is no last cart.\n"

-- | Compute and display the location of the first crash and the last car's
-- location.
main :: IO ()
main = do
    input <- getContents
    case parse cartsAndTracks "" input of
      Left err -> fail (show err)
      Right (cs, ts) -> answer (head crashed) m
          where (crashed, m) = drive ts cs

-- | Parse a point from the crop.
track :: Parser (Maybe (Either Track Direction))
track = do
    c <- oneOf " |-/\\+^>v<"
    return (f c)
        where f '|'  = Just $ Left Pipe
              f '-'  = Just $ Left Dash
              f '/'  = Just $ Left Slash
              f '\\' = Just $ Left BackSlash
              f '+'  = Just $ Left Cross
              f '^'  = Just $ Right North
              f '>'  = Just $ Right East
              f 'v'  = Just $ Right South
              f '<'  = Just $ Right West
              f _    = Nothing

-- | Flatten a two-dimensional array assigning a point to each entry.
outline :: [[a]] -> [(Point, a)]
outline = concat . zipWith expy [0..]
    where expy y = zipWith (expx y) [0..]
          expx y x a = ((x, y), a)

-- | Parse the mapped out tracks.
explore :: Parser [Either (Point, Track) (Point, Direction)]
explore = do
    ys <- sepBy (many track) newline <* eof
    return $ mapMaybe unwrap $ outline ys
        where unwrap (_, Nothing) = Nothing
              unwrap (p, Just (Left  t)) = Just $ Left  (p, t)
              unwrap (p, Just (Right d)) = Just $ Right (p, d)

-- | Parse the tracks and carts from a drawing.
cartsAndTracks :: Parser ([Cart], Tracks)
cartsAndTracks = do
    xs <- explore
    return (carts xs, tracks xs)
        where carts = map toCart . rights
              toCart (p, d) = Cart p d Leftwise
              tracks = Map.fromList . map f
              f (Left  (p, t)) = (p, t)
              f (Right (p, North)) = (p, Pipe)
              f (Right (p, South)) = (p, Pipe)
              f (Right (p, East)) = (p, Dash)
              f (Right (p, West)) = (p, Dash)
