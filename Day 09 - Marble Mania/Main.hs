module Main (main) where

import qualified Data.Map.Strict as Map
import Text.ParserCombinators.Parsec
import Text.Printf (printf)

-- | A marble number.
type Marble = Int

-- | The circle into which the Marble are placed.
--
-- The circle is either empty or it has some marble(s) to the left of the
-- current one, the current marble, and some marble(s) to the right of the
-- current one.
data Ring = Empty | Ring [Marble] Marble [Marble]
    deriving (Show)

-- | Direction in which we can change the current Marble in a Ring.
data Direction = ClockWise | CounterClockWise

-- | The Ring rotated a number of time in the given direction.
--
-- >>> rotate ClockWise 1 $ Ring [2,1] 3 [4,5]
-- Ring [1] 2 [3,4,5]
-- >>> rotate ClockWise 2 $ Ring [2,1] 3 [4,5]
-- Ring [] 1 [2,3,4,5]
-- >>> rotate ClockWise 3 $ Ring [2,1] 3 [4,5]
-- Ring [4,3,2] 5 [1]
-- >>> rotate ClockWise 9001 $ Ring [] 0 []
-- Ring [] 0 []
-- >>> rotate CounterClockWise 42 Empty
-- Empty
rotate :: Direction -> Int -> Ring -> Ring
rotate _ _ Empty = Empty
rotate _ _ xs@(Ring [] _ []) = xs
rotate _ 0 xs = xs
rotate ClockWise n (Ring (l : ls) c rs) =
    rotate ClockWise (n - 1) $ Ring ls l (c : rs)
rotate ClockWise n (Ring [] c rs) =
    rotate ClockWise n $ Ring (reverse rs) c []
rotate CounterClockWise n (Ring ls c (r : rs)) =
    rotate CounterClockWise (n - 1) $ Ring (c : ls) r rs
rotate CounterClockWise n (Ring ls c []) =
    rotate CounterClockWise n $ Ring [] c (reverse ls)

-- | Place the Marble in the given direction with respect to the current Marble
-- in the Ring. In other words, push the current Marble in the opposite
-- direction and place the given one as the current.
--
-- >>> place ClockWise 6 $ Ring [2,1] 3 [4,5]
-- Ring [2,1] 6 [3,4,5]
-- >>> place CounterClockWise 6 $ Ring [2,1] 3 [4,5]
-- Ring [3,2,1] 6 [4,5]
-- >>> place ClockWise 42 Empty
-- Ring [] 42 []
-- >>> place CounterClockWise 42 Empty
-- Ring [] 42 []
place :: Direction -> Marble -> Ring -> Ring
place _  m Empty = Ring [] m []
place ClockWise m (Ring ls c rs) = Ring ls m (c : rs)
place CounterClockWise m (Ring ls c rs) = Ring (c : ls) m rs

-- | The current marble (if any) and the Ring with it (rotated once on the
-- given direction).
--
-- >>> pick ClockWise $ Ring [2,1] 3 [4,5]
-- (Just 3,Ring [1] 2 [4,5])
-- >>> pick CounterClockWise $ Ring [2,1] 3 [4,5]
-- (Just 3,Ring [2,1] 4 [5])
-- >>> pick ClockWise $ Ring [] 1 [2,3,4]
-- (Just 1,Ring [3,2] 4 [])
-- >>> pick CounterClockWise $ Ring [1,2,3] 4 []
-- (Just 4,Ring [] 3 [2,1])
-- >>> pick ClockWise $ Ring [] 42 []
-- (Just 42,Empty)
-- >>> pick ClockWise Empty
-- (Nothing,Empty)
pick :: Direction -> Ring -> (Maybe Marble, Ring)
pick _ Empty          = (Nothing, Empty)
pick _ (Ring [] c []) = (Just c,  Empty)
pick ClockWise (Ring (l : ls) c rs) = (Just c, Ring ls l rs)
pick ClockWise (Ring [] c rs) =
    pick ClockWise $ Ring (reverse rs) c []
pick CounterClockWise (Ring ls c (r : rs)) = (Just c, Ring ls r rs)
pick CounterClockWise (Ring ls c []) =
    pick CounterClockWise $ Ring [] c (reverse ls)

-- | Player are identified by their number.
type Player = Int

-- | A Player's score.
type Score = Int

-- | The score for each Player.
type Scores = Map.Map Player Score

-- | Reset scores for a given number of players.
reset :: Player -> Scores
reset p = Map.fromList [(x, 0) | x <- [1..p]]

-- | The player to play a turn after the given one.
next :: Player -> Scores -> Player
next p scores
  | p == n = 1 -- the current player is the last one
  | otherwise = p + 1
  where n = last (Map.keys scores)

-- | The winning score after p players took turns in a game that ends after the
-- mth Marble.
--
-- >>> winning 10 1618
-- 8317
-- >>> winning 13 7999
-- 146373
-- >>> winning 17 1104
-- 2764
-- >>> winning 21 6111
-- 54718
-- >>> winning 30 5807
-- 37305
winning :: Player -> Marble -> Score
winning p m = maximum scores
    where scores = play [1..m] 1 (reset p) (Ring [] 0 [])

-- | The scores once all the marbles are placed.
play :: [Marble] -> Player -> Scores -> Ring -> Scores
play [] _ scores _ = scores
play (m : ms) p scores xs
  | mod23     = play ms (next p scores) scored rest
  | otherwise = play ms (next p scores) scores placed
  where mod23 = m `mod` 23 == 0
        (Just taken, rest) = pick ClockWise $ rotate CounterClockWise 7 xs
        scored = Map.adjust (\s -> s + m + taken) p scores
        placed = place ClockWise m $ rotate ClockWise 1 xs

-- | Display the winning Elf's score after last marble worth, and the winning
-- Elf's score if the number of the last marble were 100 times larger.
answer :: Score -> Score -> IO ()
answer s s100 = do
    printf "The winning Elf's score is %d," s
    printf " and the winning Elf's score if the number of the last marble were 100 times larger is %d.\n" s100

-- | Simulate the game and display the winning scores.
main :: IO ()
main = do
    input <- getContents
    case parse settings "" input of
      Left err     -> error (show err)
      Right (p, m) -> answer s s100
          where s    = winning p m
                s100 = winning p (m * 100)

-- | Parse the game settings, i.e. the player count and last marble's worth.
--
-- >>> parse settings "" "10 players; last marble is worth 1618 points"
-- Right (10,1618)
-- >>> parse settings "" "13 players; last marble is worth 7999 points"
-- Right (13,7999)
-- >>> parse settings "" "17 players; last marble is worth 1104 points"
-- Right (17,1104)
-- >>> parse settings "" "21 players; last marble is worth 6111 points"
-- Right (21,6111)
-- >>> parse settings "" "30 players; last marble is worth 5807 points"
-- Right (30,5807)
settings :: Parser (Int, Marble)
settings = do
    n <- spaces >> many1 digit <* string " players;"
    m <- spaces >> string "last marble is worth " >> many1 digit <* string " points"
    return (read n, read m)
