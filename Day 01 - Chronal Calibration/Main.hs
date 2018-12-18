module Main (main) where

import Data.Set hiding (foldl)
import Text.ParserCombinators.Parsec
import Text.Printf (printf)

-- | Device's frequency.
type Freq = Int

-- | A change in the device's frequency.
type Drift = Int

-- | Adjust the device's frequency with a given change.
--
-- >>> 0 `adjust` 6
-- 6
-- >>> 6 `adjust` (-1)
-- 5
adjust :: Freq -> Drift -> Freq
adjust = (+)

-- | The device's initial frequency.
initialFreq :: Freq
initialFreq = 0

-- | The measured final device's frequency given the initial frequency and a
-- sequence of changes.
--
-- >>> calibrate 0 [1, -2, 3, 1]
-- 3
-- >>> calibrate 0 [1, 1, 1]
-- 3
-- >>> calibrate 0 [1, 1, -2]
-- 0
-- >>> calibrate 0 [-1, -2, -3]
-- -6
calibrate :: Freq -> [Drift] -> Freq
calibrate = foldl adjust

-- | The first device's frequency reached twice given the initial frequency and
-- a sequence of changes to cycle through.
--
-- >>> calibrate' 0 [1, -2, 3, 1]
-- 2
-- >>> calibrate' 0 [1, -1]
-- 0
-- >>> calibrate' 0 [3, 3, 4, -2, -4]
-- 10
-- >>> calibrate' 0 [-6, 3, 8, 5, -6]
-- 5
-- >>> calibrate' 0 [7, 7, -2, -7, -4]
-- 14
calibrate' :: Freq -> [Drift] -> Freq
calibrate' f ds = dup empty freqs
    where freqs = scanl adjust f $ cycle ds
          dup s (x:xs)
              | x `member` s = x
              | otherwise    = dup (x `insert` s) xs
          dup _ [] = error "dup: empty list"

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
      Left err -> print err >> fail "parse error"
      Right xs -> answer calibrated calibrated'
          where calibrated  = calibrate  initialFreq xs
                calibrated' = calibrate' initialFreq xs

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
    _      <- spaces
    let num = read digits
     in return (if sign == '+' then num else (- num))

-- | Parse a sequence of Drift.
--
-- >>> parse drifts "" "+1, -2, +3, +1"
-- Right [1,-2,3,1]
drifts :: Parser [Drift]
drifts = sepBy drift sep <* eof
    where sep = optional (char ',') >> spaces
