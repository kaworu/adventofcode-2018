module Main (main) where

import Data.List
import Data.Map.Strict (Map)
import Text.ParserCombinators.Parsec
import Text.Printf (printf)
import qualified Data.Map.Strict as Map

-- | A coordinate of a star in the sky.
type Point = (Int, Int)

-- | The speed of a light floating in the distance.
type Velocity = (Int, Int)

-- | A star in the sky, defined by its position and speed.
data Star = Star { position :: Point, speed :: Velocity }
    deriving (Show)

-- | All the stars.
type Sky = Map Point [Star]

-- | All the points around the given point.
--
-- >>> neighbours (0, 0)
-- [(-1,-1),(0,-1),(1,-1),(1,0),(1,1),(0,1),(-1,1),(-1,0)]
neighbours :: Point -> [Point]
neighbours (x, y) = [tl, top, tr, right, br, bottom, bl, left]
    where tl     = (x - 1, y - 1)
          top    = (x,     y - 1)
          tr     = (x + 1, y - 1)
          right  = (x + 1, y    )
          br     = (x + 1, y + 1)
          bottom = (x,     y + 1)
          bl     = (x - 1, y + 1)
          left   = (x - 1, y    )

-- | The given star moved after waiting one second.
--
-- >>> move Star { position = (9, 1), speed = (0, 2) }
-- Star {position = (9,3), speed = (0,2)}
-- >>> move Star { position = (7, 0), speed = (-1, 0) }
-- Star {position = (6,0), speed = (-1,0)}
-- >>> move Star { position = (3, -2), speed = (-1, 1) }
-- Star {position = (2,-1), speed = (-1,1)}
-- >>> move Star { position = (6, 10), speed = (-2,-1) }
-- Star {position = (4,9), speed = (-2,-1)}
move :: Star -> Star
move (Star (x, y) (vx, vy)) = Star (x', y') (vx, vy)
    where (x', y') = (x + vx, y + vy)

-- | Record a star in the sky.
record :: Star -> Sky -> Sky
record s = Map.alter (appending s) (position s)
    where appending x Nothing   = Just [x]
          appending x (Just xs) = Just (x : xs)

-- | The (top-left, bottom-right) coordinates of the stars at the edges of the
-- sky.
limits :: Sky -> (Point, Point)
limits k = ((minimum xs, minimum ys), (maximum xs, maximum ys))
    where xs  = map fst pts
          ys  = map snd pts
          pts = Map.keys k

-- | What the sky look like.
look :: Sky -> String
look k = intercalate "\n" rows
    where rows    = [line y  | y <- [ymin..ymax]]
          line y  = [chr x y | x <- [xmin..xmax]]
          chr x y = if Map.member (x, y) k then '#' else '.'
          ((xmin, ymin), (xmax, ymax)) = limits k

-- | The sky after waiting one second.
next :: Sky -> Sky
next = foldr (record . move) Map.empty . concat . Map.elems

-- | True if the message has appeared in the sky, False otherwise.
appeared :: Sky -> Bool
appeared k = all hasNeighbour (Map.keys k)
    where hasNeighbour p = any (`Map.member` k) (neighbours p)

-- | The sky once the message has appeared along with the count of second to
-- wait.
wait :: Sky -> Int -> (Sky, Int)
wait k n
  | appeared k = (k, n)
  | otherwise = wait (next k) (n + 1)

-- | Display the North Pole rescue operation's message along with the count of
-- seconds to wait until the message appear.
answer :: Sky -> Int -> IO ()
answer k n = do
    printf "After %d seconds:\n" n
    printf "%s\n" (look k)

-- | Calculate and display the North Pole rescue operation's message along with
-- the count of seconds to wait until the message appear.
main :: IO ()
main = do
    input <- getContents
    case parse sky "" input of
      Left err -> fail (show err)
      Right k  -> let (k', n) = wait k 0 in answer k' n

-- | Parse a negative or positive number.
--
-- >>> parse number "" "42"
-- Right 42
-- >>> parse number "" " -1"
-- Right (-1)
-- >>> parse number "" "0"
-- Right 0
-- >>> parse number "" "-0"
-- Right 0
number :: Parser Int
number = negative <|> positive
    where negative = do
            digits <- spaces >> char '-' >> many1 digit
            return (- read digits)
          positive = do
            digits <- spaces >> many1 digit
            return (read digits)

-- | Parse two numbers as coordinate (separated by comma).
--
-- >>> parse point "" " 9,  1"
-- Right (9,1)
-- >>> parse point "" " 0,  2"
-- Right (0,2)
-- >>> parse point "" " 7,  0"
-- Right (7,0)
-- >>> parse point "" "-1,  0"
-- Right (-1,0)
point :: Parser Point
point = do
    x <- spaces >> number
    _ <- char ','
    y <- spaces >> number
    return (x, y)

-- | Parse a recorded star position and velocity.
--
-- >>> parse star "" "position=< 9,  1> velocity=< 0,  2>"
-- Right (Star {position = (9,1), speed = (0,2)})
-- >>> parse star "" "position=< 7,  0> velocity=<-1,  0>"
-- Right (Star {position = (7,0), speed = (-1,0)})
-- >>> parse star "" "position=< 3, -2> velocity=<-1,  1>"
-- Right (Star {position = (3,-2), speed = (-1,1)})
star :: Parser Star
star = do
    p <- spaces >> string "position=<" >> point <* char '>'
    v <- spaces >> string "velocity=<" >> point <* char '>'
    spaces
    return Star { position = p, speed = v }

-- | Parse the whole sky.
sky :: Parser Sky
sky = do
    xs <- many1 star
    return $ foldr record Map.empty xs
