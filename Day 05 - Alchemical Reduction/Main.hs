module Main (main) where

import Data.Char
import Data.List
import Data.Ord
import qualified Data.Set as S
import Text.Printf (printf)

-- | Small units forming a Polymer.
type Unit = Char

-- | Long unit sequence composing the suit's material.
type Polymer = [Unit]

-- | Unit polarity conversion.
--
-- >>> positive 'a'
-- 'a'
-- >>> positive 'A'
-- 'a'
-- >>> negative 'a'
-- 'A'
-- >>> negative 'A'
-- 'A'
-- >>> normal 'a'
-- 'a'
-- >>> normal 'A'
-- 'a'
positive, negative, normal :: Unit -> Unit
positive = toLower
negative = toUpper
normal   = positive

-- | The unit of the same type and opposite polarity.
--
-- >>> opposite 'a'
-- 'A'
-- >>> opposite 'A'
-- 'a'
opposite :: Unit -> Unit
opposite x
  | isUpper x = positive x
  | otherwise = negative x

-- | True if x and y are of the same type and opposite polarity,
-- False otherwise.
--
-- >>> trigger 'r' 'R'
-- True
-- >>> trigger 'A' 'a'
-- True
-- >>> trigger 'r' 's'
-- False
-- >>> trigger 'B' 'B'
-- False
trigger :: Unit -> Unit -> Bool
trigger x y = x == opposite y

-- | Normalized units of the given Polymer.
--
-- >>> units "dabAcCaCBAcCcaDA"
-- fromList "abcd"
units :: Polymer -> S.Set Unit
units = foldr S.insert S.empty . map normal

-- | The Polymer excluding the units of the same type as the given Unit.
--
-- >>> remove 'a' "dabAcCaCBAcCcaDA"
-- "dbcCCBcCcD"
-- >>> remove 'A' "dabAcCaCBAcCcaDA"
-- "dbcCCBcCcD"
-- >>> remove 'b' "dabAcCaCBAcCcaDA"
-- "daAcCaCAcCcaDA"
-- >>> remove 'c' "dabAcCaCBAcCcaDA"
-- "dabAaBAaDA"
-- >>> remove 'D' "dabAcCaCBAcCcaDA"
-- "abAcCaCBAcCcaA"
remove :: Unit -> Polymer -> Polymer
remove x = filter (\u -> normal u /= normal x)

-- | The Polymer's fully reacted form.
--
-- >>> react "aA"
-- ""
-- >>> react "abBA"
-- ""
-- >>> react "aabAAB"
-- "aabAAB"
-- >>> react "dabAcCaCBAcCcaDA"
-- "dabCBAcaDA"
react :: Polymer -> Polymer
react []  = []
react [x] = [x]
react (x : xs)
  | null xs'      = [x]
  | x `trigger` y = ys
  | otherwise     = x : xs'
  where xs' = react xs
        (y:ys) = xs'

-- | Display how many units remain after fully reacting the Polymer and its
-- shortest improved version.
answer :: Polymer -> Polymer -> IO ()
answer p s = do
    printf "%d units remain after the polymer has fully reacted,\n" (length p)
    printf "and the shortest improved polymer contains %d units.\n" (length s)

-- | Compute and display how many units remain after fully reacting the Polymer
-- and its shortest improved version.
main :: IO ()
main = do
    input <- getContents
    let reacted = react (rstrip input)
        removed = map (\u -> remove u reacted) (S.elems $ units reacted)
        shortest = minimumBy (comparing length) $ map react removed
     in answer reacted shortest

-- | The given String without trailing whitespace characters.
--
-- see https://stackoverflow.com/a/3373478
--
-- >>> rstrip "Hello\nWorld!\n"
-- "Hello\nWorld!"
rstrip :: String -> String
rstrip = reverse . dropWhile isSpace . reverse
