module Main (main) where

import Data.Function (on)
import Data.List
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Set (Set)
import Text.ParserCombinators.Parsec
import Text.Printf (printf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | Swap two elements from a Map.
--
-- If any index are not member of the Map, or if they are equal then the Map is
-- unchanged.
--
-- >>> swap 1 2 $ Map.fromList [(1,"one"),(2,"two")]
-- fromList [(1,"two"),(2,"one")]
-- >>> swap 1 42 $ Map.fromList [(1,"one"),(2,"two")]
-- fromList [(1,"one"),(2,"two")]
-- >>> swap 1 1 $ Map.fromList [(1,"one"),(2,"two")]
-- fromList [(1,"one"),(2,"two")]
-- >>> swap 42 1 $ Map.fromList [(1,"one"),(2,"two")]
-- fromList [(1,"one"),(2,"two")]
swap :: (Ord k) => k -> k -> Map k a -> Map k a
swap x y m = case (Map.lookup x m, Map.lookup y m) of
               (Just x', Just y') -> Map.insert y x' $ Map.insert x y' m
               _                  -> m

-- | A (y, x) coordinate in the map.
--
-- NOTE: in this puzzle we use (y, x) instead of the more traditional (x, y)
-- representation to honor the reading order, hence the name change (i.e. Spot
-- instead of Point).
type Spot = (Int, Int)

-- | Hit Points.
type Hp = Int

-- | Attack Hit Points.
type Power = Hp

-- | Goblins versus Elves.
data Kind = Goblin | Elf
    deriving (Eq, Ord)

-- | An area location.
data Location = Wall | Cavern | Occupied Kind Hp
    deriving (Eq)

-- | The whole scanned map.
type Cave = Map Spot Location

-- | Display Goblins and Elves as described in the README.
instance Show Kind where
    show Goblin = "G"
    show Elf    = "E"

-- | Display map locations as described in the README.
instance Show Location where
    show Wall   = "#"
    show Cavern = "."
    show (Occupied kind _) = show kind

-- | Initial Hit Points for both Goblins and Elves.
initialHp :: Hp
initialHp = 200

-- | Attack Power of both Goblins and Elves.
attackPower :: Power
attackPower = 3

-- | The enemy Kind of the given one.
enemy :: Kind -> Kind
enemy Goblin = Elf
enemy Elf    = Goblin

-- | The four reachable spots around the given one, in reading order.
--
-- >>> neighbours (0, 0)
-- [(-1,0),(0,-1),(0,1),(1,0)]
-- >>> neighbours (-1, 42)
-- [(-2,42),(-1,41),(-1,43),(0,42)]
neighbours :: Spot -> [Spot]
neighbours (y, x) = [(y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x)]

-- | True if the given location is occupied (regardless of the Kind),
-- False otherwise.
--
-- >>> isOccupied Wall
-- False
-- >>> isOccupied Cavern
-- False
-- >>> isOccupied (Occupied Elf initialHp)
-- True
-- >>> isOccupied (Occupied Goblin initialHp)
-- True
isOccupied :: Location -> Bool
isOccupied (Occupied _ _) = True
isOccupied _              = False

-- | True if the given location occupied by a unit of the given Kind,
-- False otherwise.
--
-- >>> isOccupiedBy Elf Wall
-- False
-- >>> isOccupiedBy Goblin Cavern
-- False
-- >>> isOccupiedBy Goblin (Occupied Elf initialHp)
-- False
-- >>> isOccupiedBy Elf (Occupied Goblin initialHp)
-- False
-- >>> isOccupiedBy Elf (Occupied Elf initialHp)
-- True
-- >>> isOccupiedBy Goblin (Occupied Goblin initialHp)
-- True
isOccupiedBy :: Kind -> Location -> Bool
isOccupiedBy k (Occupied k' _) = k' == k
isOccupiedBy _ _               = False

-- | The Cavern neighbours spots from the given one, in reading order.
--
-- >>> freeNeighbours (Map.fromList [((0,1), Cavern)]) (0, 0)
-- [(0,1)]
-- >>> freeNeighbours (Map.fromList [((1,1), Cavern)]) (0, 0)
-- []
-- >>> freeNeighbours (Map.fromList [((0,1), Wall)]) (0, 0)
-- []
-- >>> freeNeighbours Map.empty (0, 0)
-- []
freeNeighbours :: Cave -> Spot -> [Spot]
freeNeighbours cv = filter free . neighbours
    where free spot = case Map.lookup spot cv of
                        Just Cavern -> True
                        _           -> False

-- | Every unit from the given Kind, in reading order.
units :: Kind -> Cave -> Cave
units kind = Map.filter (isOccupiedBy kind)

-- | the Cave ticked until the stop condition is met, along with the count of
-- full round taken.
combat :: (Cave -> Bool) -> Cave -> (Cave, Int)
combat cond = combat' 0
    where combat' i cv
            | cond cv   = (cv, i)
            | otherwise = combat' (i + 1) (tick cv)

-- | the Cave after a combat without mercy, along with the count of full round
-- taken.
combatUntilOneSideWin :: Cave -> (Cave, Int)
combatUntilOneSideWin = combat ((==1) . Set.size . kindLeft)

-- | The list of Kind left in the Cave.
kindLeft :: Cave -> Set Kind
kindLeft = Set.fromList . mapMaybe occupiedBy . Map.elems
    where occupiedBy (Occupied kind _) = Just kind
          occupiedBy _ = Nothing

-- | The Cave after one round, i.e. once every unit has moved and attacked
-- (in reading order).
tick :: Cave -> Cave
tick cv = foldl advance cv everybody
    where everybody = Map.keys $ Map.filter isOccupied cv

-- | The Cave after the unit at the given Spot has taken one round of action.
advance :: Cave -> Spot -> Cave
advance cv spot =
    case Map.lookup spot cv of
      Just (Occupied kind _) -> moveThenAttack kind
      _ -> cv -- no changes
     where moveThenAttack kind = attack (enemy kind) destination afterMoving
            where afterMoving = move spot destination cv
                  destination = pathTo (enemy kind)
                  pathTo target = path cv spot (spotsOfInterest target spot cv)

-- | The Set of Spot from where we can attack the given Kind of enemy.
--
-- The given spot is the current position, which can be one of interest without
-- being a Cavern.
spotsOfInterest :: Kind -> Spot -> Cave -> Set Spot
spotsOfInterest kind here cv = Set.fromList inRange
    where inRange = filter isHereOrCavern interstingSpots
          isHereOrCavern spot = isHere spot || isCavern spot
          isHere spot = spot == here
          isCavern spot = Map.lookup spot cv == Just Cavern
          interstingSpots = concatMap neighbours enemySpots
          enemySpots = Map.keys $ units kind cv

-- | The neighbours Spot from start to step into in order to reach a Spot of
-- interest.
path :: Cave -> Spot -> Set Spot -> Spot
path cv start soi
  | Set.null soi = start -- no point of interest
  | start `Set.member` soi = start -- we're already sitting at one
  | otherwise = case freeNeighbours cv start of
                  [] -> start -- can't move
                  xs -> bfs (Set.fromList $ start : xs) [(x, x) | x <- xs] [] []
  -- Breadth-first search for the perfect Spot.
  --
  -- Path are stored with only the neighbours starting point and landing point
  -- as we only need to know the first step to take in order to reach a spot of
  -- interest.
  --
  -- We are careful to preserve the reading order in the current working set
  -- (because we add the spots seen in visited in order), next working set, and
  -- found. This way we get the reading order "for free" (almost because we're
  -- ++ing a lot).
  where bfs _       [] []   []    = start -- nowhere to go
        bfs visited [] next []    = bfs visited next [] []
        bfs _       [] _    found = fst $ head found
        bfs visited ((s, s') : ss) next found
          | s' `Set.member` soi = bfs visited ss next (found ++ [(s, s')])
          | otherwise = let around = freeNeighbours cv s'
                            steps = filter (`Set.notMember` visited) around
                            visited' = Set.union visited $ Set.fromList steps
                            next' = next ++ [(s, x) | x <- steps] in
                            bfs visited' ss next' found

-- | The Cave once someone move from an occupied to a cavern.
--
-- NOTE: Simply swap the two Location in the Cave without checking wether the
-- starting Spot is Occupied nor the landing Spot is a Cavern.
move :: Spot -> Spot -> Cave -> Cave
move from to cv
  | from == to = cv
  | otherwise  = swap from to cv

-- | The Cave once the weakest nearby enemy of the given Kind has been attacked
-- from the given Spot.
attack :: Kind -> Spot -> Cave -> Cave
attack kind from cv =
    case weakest targets of
      Nothing -> cv
      Just (spot, hp) -> Map.insert spot (injuredOrDead hp) cv
    where weakest [] = Nothing
          weakest xs = Just $ minimumBy (compare `on` snd) xs
          targets = mapMaybe (attackable . locate) (neighbours from)
          locate spot = (spot, Map.lookup spot cv)
          attackable (spot, Just (Occupied k hp))
            | k == kind = Just (spot, hp)
            | otherwise = Nothing
          attackable _ = Nothing
          injuredOrDead hp
            | hp <= attackPower = Cavern -- died
            | otherwise = Occupied kind (hp - attackPower) -- injured

-- | Display the count of rounds, winner kind with total hp left, and outcome.
answer :: Int -> Cave -> IO ()
answer i cv = do
    printf "%s\n\n" (display cv)
    printf "Combat ends after %d full rounds\n" i
    printf "%s win with %d total hit points left\n" (k2s winningKind) totalHp
    printf "Outcome: %d * %d = %d\n" i totalHp (i * totalHp)
    where winningKind = head $ Set.elems (kindLeft cv)
          k2s Goblin = "Goblins"
          k2s Elf    = "Elves"
          totalHp = sum $ mapMaybe hp $ Map.elems cv
              where hp (Occupied _ hpLeft) = Just hpLeft
                    hp _ = Nothing

-- | Compute and display the count of rounds, winner kind with total hp left,
-- and outcome.
main :: IO ()
main = do
    input <- getContents
    case parse cave "" input of
      Left err -> fail (show err)
      Right cv -> let (done, i) = combatUntilOneSideWin cv in
                      answer i done

-- | Parse a location
--
-- >>> parse location "" "#"
-- Right #
-- >>> parse location "" "."
-- Right .
-- >>> parse location "" "G"
-- Right G
-- >>> parse location "" "E"
-- Right E
-- >>> parse location "" "?"
-- Left (line 1, column 1):
-- unexpected "?"
location :: Parser Location
location = do
    c <- oneOf "#.GE"
    return $ f c
        where f '#' = Wall
              f '.' = Cavern
              f 'G' = Occupied Goblin initialHp
              f 'E' = Occupied Elf    initialHp
              f x   = error (x : ": invalid location")

-- | Parse the whole map as a list of Spot and Location.
explore :: Parser [(Spot, Location)]
explore = do
    xs <- sepBy (many location) newline <* eof
    return $ outline xs

-- | Parse the whole map as a Cave
cave :: Parser Cave
cave = do
    xs <- explore
    return $ simplify $ Map.fromList xs

-- | The same Cave without unecessary Walls.
simplify :: Cave -> Cave
simplify cv = Map.filterWithKey f cv
    where f s Wall = any (/= Wall) (around s)
          f _ _    = True
          around s = mapMaybe (`Map.lookup` cv) (neighbours s)

-- | Flatten a two-dimensional array assigning a spot to each entry.
--
-- NOTE: The resulting array is generated in reading order.
outline :: [[a]] -> [(Spot, a)]
outline = concat . zipWith expy [0..]
    where expy y = zipWith (expx y) [0..]
          expx y x a = ((y, x), a)

-- | A string representation of the Cave as described in the README.
display :: Cave -> String
display = displayf (const Nothing)

-- | A string representation of the Cave as described in the README with some
-- tweaks.
--
-- The given closure allow to override some Spot in order to highlight them,
-- e.g. the "Reachable" or "Nearest" spots.
displayf :: (Spot -> Maybe String) -> Cave -> String
displayf f cv = intercalate "\n" $ ydisplay f 0 ymax cv
    where ymax = maximum $ map fst $ Map.keys cv

-- | Show the Cave rows.
ydisplay :: (Spot -> Maybe String) -> Int -> Int -> Cave -> [String]
ydisplay f y ymax cv
  | y > ymax  = []
  | otherwise = xdisplay f (y, 0) xmax ycv : ydisplay f (y + 1) ymax cv
  where xmax = maximum $ map snd $ Map.keys ycv
        ycv  = Map.filterWithKey (\(y', _) _ -> y' == y) cv

-- | Show the Cave line.
xdisplay :: (Spot -> Maybe String) -> Spot -> Int -> Cave -> String
xdisplay f (y, x) xmax cv
  | x > xmax  = "   " ++ udisplay cv
  | otherwise = loc (Map.lookup (y, x) cv) ++ xdisplay f (y, x + 1) xmax cv
  where loc Nothing  = " "
        loc (Just l) = fromMaybe (show l) (f (y, x))

-- | Show the Cave Goblins and Elves.
udisplay :: Cave -> String
udisplay = intercalate ", " . mapMaybe kindAndHp . Map.elems
    where kindAndHp (Occupied kind hp) = Just $ printf "%s(%d)" (show kind) hp
          kindAndHp _ = Nothing
