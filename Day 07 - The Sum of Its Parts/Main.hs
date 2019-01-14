module Main (main) where

import Data.List
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Text.ParserCombinators.Parsec
import Text.Printf (printf)

-- | A step from the Sleigh kit assembly instructions.
type Step = Char

-- | The Sleigh kit assembly instructions. Keys are the steps and values the
-- step(s) they depend on.
type Instructions = Map.Map Step (Set.Set Step)

-- | The first step without dependency.
--
-- >>> candidate $ Map.fromList [('A', Set.fromList "C"), ('C', Set.fromList "")]
-- 'C'
-- >>> candidate $ Map.fromList []
-- ... empty list
candidate :: Instructions -> Step
candidate = head . Map.keys . Map.filter Set.null

-- | The instructions left once the given step is finished.
--
-- >>> finished 'C' $ Map.fromList [('A', Set.fromList "C"), ('C', Set.fromList "")]
-- fromList [('A',fromList "")]
-- >>> finished 'A' $ Map.fromList []
-- fromList []
finished :: Step -> Instructions -> Instructions
finished s = Map.map (Set.delete s) . Map.delete s

-- | The step to perform to follow the instructions, in order.
--
-- >>> order $ Map.fromList [('A', Set.fromList "C"), ('C', Set.fromList "")]
-- "CA"
-- >>> order $ Map.fromList []
-- ""
order :: Instructions -> [Step]
order xs
  | xs == Map.empty = []
  | otherwise = let c = candidate xs in c : order (finished c xs)

-- | Display the steps to be performed, in order, from the Sleigh kit assembly
-- instructions.
answer :: [Step] -> IO ()
answer = printf "The steps should be completed in the order %s.\n"

-- | Compute and display the steps to be performed, in order, from the Sleigh
-- kit assembly instructions.
main :: IO ()
main = do
    input <- getContents
    case parse instructions "" input of
      Left err -> error (show err)
      Right xs -> answer (order xs)

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
          ins s = Just . maybe (Set.singleton s) (Set.insert s)
          create = Just . fromMaybe Set.empty
