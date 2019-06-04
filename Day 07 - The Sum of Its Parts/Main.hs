module Main (main) where

import Data.Char
import Data.List
import Data.Map.Strict (Map)
import Data.Maybe
import Text.ParserCombinators.Parsec
import Text.Printf (printf)
import qualified Data.Map.Strict as Map

-- | A step from the Sleigh kit assembly instructions.
type Step = Char

-- | The Sleigh kit assembly instructions. Keys are the steps and values the
-- step(s) they depend on.
type Instructions = Map Step [Step]

-- | The steps having no dependency from the given instructions, in
-- alphabetical order.
--
-- >>> candidates $ Map.fromList [('A',"C"),('B',"A"),('C',""),('D',"A"),('E',"FDB"),('F',"C")]
-- "C"
-- >>> candidates $ Map.fromList [('A',""),('B',"A"),('D',"A"),('E',"FDB"),('F',"")]
-- "AF"
-- >>> candidates $ Map.fromList [('B',""),('D',""),('E',"FDB"),('F',"")]
-- "BDF"
-- >>> candidates $ Map.fromList [('D',""),('E',"FD"),('F',"")]
-- "DF"
-- >>> candidates $ Map.fromList [('E',"F"),('F',"")]
-- "F"
-- >>> candidates $ Map.fromList [('E',"")]
-- "E"
-- >>> candidates $ Map.fromList []
-- ""
candidates :: Instructions -> [Step]
candidates = Map.keys . Map.filter null

-- | The instructions left once a given step is finished.
--
-- >>> finished 'C' $ Map.fromList [('A',"C"),('B',"A"),('C',""),('D',"A"),('E',"FDB"),('F',"C")]
-- fromList [('A',""),('B',"A"),('D',"A"),('E',"FDB"),('F',"")]
-- >>> finished 'A' $ Map.fromList [('A',""),('B',"A"),('D',"A"),('E',"FDB"),('F',"")]
-- fromList [('B',""),('D',""),('E',"FDB"),('F',"")]
-- >>> finished 'B' $ Map.fromList [('B',""),('D',""),('E',"FDB"),('F',"")]
-- fromList [('D',""),('E',"FD"),('F',"")]
-- >>> finished 'D' $ Map.fromList [('D',""),('E',"FD"),('F',"")]
-- fromList [('E',"F"),('F',"")]
-- >>> finished 'F' $ Map.fromList [('E',"F"),('F',"")]
-- fromList [('E',"")]
-- >>> finished 'E' $ Map.fromList [('E',"")]
-- fromList []
-- >>> finished 'X' $ Map.fromList []
-- fromList []
finished :: Step -> Instructions -> Instructions
finished x = Map.map (delete x) . Map.delete x

-- | Unit of time spent to perform a step from the instructions.
type Second = Int

-- | Count of seconds required to finish a given step.
--
-- >>> time 'A'
-- 61
-- >>> time 'Z'
-- 86
time :: Step -> Second
time x = ord x - ord 'A' + 61

-- | A person working on a step, i.e. you or an elf.
--
-- A worker is either idle (has no step to perform), working on a step until a
-- given time, or has finished the step it was working at a given time.
data Worker = Idle | Working Step Second | Worked Step Second
    deriving (Show)

-- | A given number of idle workers.
--
-- >>> workers 6
-- [Idle,Idle,Idle,Idle,Idle,Idle]
-- >>> workers 0
-- []
workers :: Int -> [Worker]
workers n = replicate n Idle

-- | Stop a worker if it has finished its step, make it idle if it has
-- previously finished its step and is not working on another one.
--
-- >>> relax 42 $ Idle
-- Idle
-- >>> relax 42 $ Worked 'A' 12
-- Idle
-- >>> relax 42 $ Working 'A' 42
-- Worked 'A' 42
-- >>> relax 42 $ Working 'A' 41
-- Worked 'A' 41
-- >>> relax 42 $ Working 'A' 43
-- Working 'A' 43
relax :: Second -> Worker -> Worker
relax _ Idle         = Idle
relax _ (Worked _ _) = Idle
relax now (Working st sec)
  | sec <= now = Worked  st sec
  | otherwise  = Working st sec

-- | The step that was realized by the given worker along with the last second
-- used to complete it, if any.
--
-- >>> realization $ Idle
-- Nothing
-- >>> realization $ Working 'A' 42
-- Nothing
-- >>> realization $ Worked 'A' 42
-- Just ('A',41)
realization :: Worker -> Maybe (Step, Second)
realization (Worked st sec) = Just (st, sec - 1)
realization _               = Nothing

-- | The step that is currently worked on, if any.
--
-- >>> ongoing $ Idle
-- Nothing
-- >>> ongoing $ Working 'A' 42
-- Just 'A'
-- >>> ongoing $ Worked 'A' 42
-- Nothing
ongoing :: Worker -> Maybe Step
ongoing (Working st _) = Just st
ongoing _              = Nothing

-- | Attempt to assign a new step to each non-Working workers.
--
-- >>> assign 0 ['A'] [Idle]
-- [Working 'A' 61]
-- >>> assign 0 ['A'] [Working 'B' 42]
-- [Working 'B' 42]
-- >>> assign 0 ['A'] [Worked 'B' 0]
-- [Working 'A' 61]
-- >>> assign 0 ['A', 'B'] [Idle,Idle]
-- [Working 'A' 61,Working 'B' 62]
-- >>> assign 0 ['A', 'B'] [Idle,Idle,Idle]
-- [Working 'A' 61,Working 'B' 62,Idle]
-- >>> assign 0 ['A', 'B', 'C'] [Working 'D' 100, Worked 'Z' 0, Idle]
-- [Working 'D' 100,Working 'A' 61,Working 'B' 62]
assign :: Second -> [Step] -> [Worker] -> [Worker]
assign _ _  [] = []
assign _ [] ws = ws
assign now (x : xs) (Idle : ws) =
    Working x (now + time x) : assign now xs ws
assign now (x : xs) (Worked _ _ : ws) =
    Working x (now + time x) : assign now xs ws
assign now (x : xs) (w : ws) = w : assign now (x : xs) ws

-- | The next second a worker will have realized a step.
--
-- >>> next 0 [Working 'A' 42]
-- 42
-- >>> next 0 [Working 'A' 42, Working 'B' 12, Working 'C' 100]
-- 12
next :: Second -> [Worker] -> Second
next now = minimum . filter (>now) . mapMaybe working
    where working (Working _ sec) = Just sec
          working _               = Nothing

-- | The realized steps and the second it was finished.
schedule :: Second -> Instructions -> [Worker] -> [(Step, Second)]
schedule now ins ws
  | ins == Map.empty = []
  | otherwise = realized ++ schedule hop todo assigned
  where relaxed = map (relax now) ws
        realized = mapMaybe realization relaxed
        todo = foldr (finished . fst) ins realized
        available = candidates todo \\ mapMaybe ongoing ws
        assigned = assign now available relaxed
        hop = next now assigned

-- | Display the steps to be performed (in order) from the Sleigh kit assembly
-- instructions when working alone, and the count of seconds required to
-- complete all of the steps when helped by four elves.
answer :: [Step] -> Second -> IO ()
answer xs t = do
    printf "The steps should be completed in the order %s," xs
    printf " and it will take %d seconds to complete all of the steps.\n" t

-- | Compute and display the steps to be performed (in order) from the Sleigh
-- kit assembly instructions when working alone, and the count of seconds
-- required to complete all of the steps when helped by four elves.
main :: IO ()
main = do
    input <- getContents
    case parse instructions "" input of
      Left err -> fail (show err)
      Right xs -> answer (order alone) (elapsed helped + 1)
          where order = map fst . schedule 0 xs
                elapsed = maximum . map snd . schedule 0 xs
                alone = workers 1
                helped = workers 5

-- | Parse line from the Sleigh kit assembly, a step dependency.
--
-- >>> parse dependency "" "Step C must be finished before step A can begin."
-- Right ('C','A')
-- >>> parse dependency "" "Step A must be finished before step B can begin."
-- Right ('A','B')
dependency :: Parser (Step, Step)
dependency = do
    x <- string "Step " >> letter
    y <- string " must be finished before step " >> letter
    _ <- string " can begin."
    spaces
    return (x, y)

-- | Parse the Sleigh kit assembly instructions.
instructions :: Parser Instructions
instructions = do
    ds <- many1 dependency
    return $ foldl' depend Map.empty ds
    where depend m (x, y) = Map.alter (ins x) y $ Map.alter create x m
          ins s = Just . maybe [s] (s:)
          create = Just . fromMaybe []
