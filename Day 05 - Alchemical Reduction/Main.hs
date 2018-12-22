module Main (main) where

import Data.Char
import Text.Printf (printf)

-- | Small units forming a Polymer.
type Unit = Char

-- | Long unit sequence composing the suit's material.
type Polymer = [Unit]

-- | True if x and y are of the same type and opposite polarity,
-- False otherwise.
--
-- >>> react 'r' 'R'
-- True
-- >>> react 'A' 'a'
-- True
-- >>> react 'r' 's'
-- False
-- >>> react 'B' 'B'
-- False
react :: Unit -> Unit -> Bool
react x y = abs (ord x - ord y) == 32

-- | The Polymer's fully reacted form.
--
-- >>> reduce "aA"
-- ""
-- >>> reduce "abBA"
-- ""
-- >>> reduce "aabAAB"
-- "aabAAB"
-- >>> reduce "dabAcCaCBAcCcaDA"
-- "dabCBAcaDA"
reduce :: Polymer -> Polymer
reduce []  = []
reduce [x] = [x]
reduce (x : xs)
  | null xs'    = [x]
  | x `react` y = ys
  | otherwise   = x : xs'
  where xs' = reduce xs
        (y:ys) = xs'

-- | Display how many units remain after fully reacting the given Polymer.
answer :: Polymer -> IO ()
answer p = do
    printf "%d units remain after the polymer has fully reacted.\n" (length p)

-- | Compute and display how many units remain after fully reacting the given
-- Polymer.
main :: IO ()
main = do
    input <- getContents
    answer $ reduce (rstrip input)

-- | The given String without trailing whitespace characters.
--
-- see https://stackoverflow.com/a/3373478
--
-- >>> rstrip "Hello\nWorld!\n"
-- "Hello\nWorld!"
rstrip :: String -> String
rstrip = reverse . dropWhile isSpace . reverse
