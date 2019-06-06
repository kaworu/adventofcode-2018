module Main (main) where

import Text.ParserCombinators.Parsec
import Text.Printf (printf)
import qualified Data.Set as Set

-- | Device's frequency.
type Freq = Int

-- | A change in the device's frequency.
type Drift = Int

-- | The frequency sequence starting from zero.
--
-- >>> freqs [1, -2, 3, 1]
-- [0,1,-1,2,3]
freqs :: [Drift] -> [Freq]
freqs = scanl (+) 0

-- | The resulting frequency after all of the changes in frequency have been
-- applied.
--
-- >>> final [1, -2, 3, 1]
-- 3
-- >>> final [1, 1, 1]
-- 3
-- >>> final [1, 1, -2]
-- 0
-- >>> final [-1, -2, -3]
-- -6
final :: [Drift] -> Freq
final = last . freqs

-- | The first frequency reached twice.
--
-- >>> dup [1, -1]
-- 0
-- >>> dup [3, 3, 4, -2, -4]
-- 10
-- >>> dup [-6, 3, 8, 5, -6]
-- 5
-- >>> dup [7, 7, -2, -7, -4]
-- 14
dup :: [Drift] -> Freq
dup = record Set.empty . freqs . cycle
    where record bag inf
            | x `Set.member` bag = x
            | otherwise = record (x `Set.insert` bag) xs
            where (x, xs) = (head inf, tail inf)

-- | Display the answers given the resulting frequency and the first frequency
-- reached twice.
answer :: Freq -> Freq -> IO ()
answer res twice = do
    printf "The resulting frequency is %d," res
    printf " and the first frequency reached twice is %d.\n" twice

-- | Compute and the calibrated device's frequency.
main :: IO ()
main = do
    input <- getContents
    case parse drifts "" input of
      Left err -> fail (show err)
      Right xs -> answer calibrated calibrated'
          where calibrated  = final xs
                calibrated' = dup xs

-- | Parse a Drift value.
--
-- >>> parse drift "" "+3"
-- Right 3
-- >>> parse drift "" "-6"
-- Right (-6)
-- >>> parse drift "" "+42  "
-- Right 42
drift :: Parser Drift
drift = do
    sign   <- oneOf "+-"
    digits <- many1 digit
    spaces
    let num = read digits
     in return (if sign == '+' then num else (- num))

-- | Parse a sequence of Drift.
--
-- >>> parse drifts "" "+1, -2, +3, +1"
-- Right [1,-2,3,1]
drifts :: Parser [Drift]
drifts = sepBy drift sep <* eof
    where sep = optional (char ',') >> spaces
