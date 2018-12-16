module Main (main) where

import Data.Set
import Text.Printf (printf)

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

-- | Device's frequency.
type Freq = Int

-- | A change in the device's frequency.
data Drift = Inc Freq | Dec Freq
    deriving (Show)

-- | Calibrate the device's frequency with a given change.
--
-- >>> 0 `calibrate` (Inc 6)
-- 6
-- >>> 6 `calibrate` (Dec 1)
-- 5
calibrate :: Freq -> Drift -> Freq
calibrate freq (Inc delta) = freq + delta
calibrate freq (Dec delta) = freq - delta

-- | The resulting frequency after all the changes have been applied.
data FinalFreq = Measuring | Measured Freq
    deriving (Show)

-- | Measure the resulting frequency after all the changes have been applied.
--
-- >>> Measuring `measure` 42
-- Measured 42
-- >>> (Measured 42) `measure` 9001
-- Measured 42
measure :: FinalFreq -> Freq -> FinalFreq
measure (Measured fin) _ = Measured fin
measure Measuring fin = Measured fin

-- | The first frequency reached twice.
data TwiceFreq = Empty | Searching (Set Freq) | Found Freq
    deriving (Show)

-- | Look for the first frequency reached twice.
--
-- >>> Empty `search` 42
-- Searching (fromList [42])
-- >>> (Searching $ fromList [42]) `search` 9001
-- Searching (fromList [42,9001])
-- >>> (Searching $ fromList [42, 9001]) `search` 42
-- Found 42
-- >>> (Found 42) `search` 0
-- Found 42
search :: TwiceFreq -> Freq -> TwiceFreq
search (Found f) _ = Found f
search (Searching s) f
  | f `member` s = Found f
  | otherwise    = Searching (f `insert` s)
search Empty f = Searching (singleton f)

-- | Given a initial frequency, a complete list of changes, and a rest of list
-- of changes, measure the resulting frequency after all the changes have been
-- applied and find the first frequency reached twice.
compute :: FinalFreq -> TwiceFreq -> Freq -> [Drift] -> [Drift] -> (Freq, Freq)
compute (Measured ff) (Found t2) _ _ _ = (ff, t2)
compute fin twice f ds [] = compute fin' twice f ds ds
    where fin' = fin `measure` f
compute fin twice f ds (x:xs) = compute fin twice' f' ds xs
    where twice' = search twice f
          f' = f `calibrate` x

-- | Given a initial frequency and a list of changes, measure the resulting
-- frequency after all the changes have been applied and find the first
-- frequency reached twice.
--
-- >>> compute' Measuring Empty initialFreq [Inc 1, Dec 2, Inc 3, Inc 1]
-- (3,2)
compute' :: FinalFreq -> TwiceFreq -> Freq -> [Drift] -> (Freq, Freq)
compute' fin twice f xs = compute fin twice f xs xs

-- | The device's initial frequency.
initialFreq :: Freq
initialFreq = 0

-- | The measured final device's frequency given a string representation of a
-- sequence of changes.
--
-- >>> part1 "+1, -2, +3, +1"
-- Right 3
-- >>> part1 "+1, +1, +1"
-- Right 3
-- >>> part1 "+1, +1, -2"
-- Right 0
-- >>> part1 "-1, -2, -3"
-- Right (-6)
part1 :: String -> Either ParseError Freq
part1 input = either Left (Right . part1') parsed
    where part1' = fst . compute' Measuring (Found 0) initialFreq
          parsed = parse drifts "" input


-- | The first device's frequency reached twice given a string representation
-- of a sequence of changes to cycle through.
--
-- >>> part2 "+1, -2, +3, +1"
-- Right 2
-- >>> part2 "+1, -1"
-- Right 0
-- >>> part2 "+3, +3, +4, -2, -4"
-- Right 10
-- >>> part2 "-6, +3, +8, +5, -6"
-- Right 5
-- >>> part2 "+7, +7, -2, -7, -4"
-- Right 14
part2 :: String -> Either ParseError Freq
part2 input = either Left (Right . part2') parsed
    where part2' = snd . compute' (Measured 0) Empty initialFreq
          parsed = parse drifts "" input

-- | Display the answers given the resulting frequency and the first frequency
-- reached twice.
answer :: Freq -> Freq -> IO ()
answer = printf "The resulting frequency is %d and the first frequency reached twice is %d.\n"

-- | Compute and the calibrated device's frequency.
main :: IO ()
main = do
    input <- getContents
    case parse drifts "" input of
      Left err -> print err >> fail "parse error"
      Right ds -> answer (fst freqs) (snd freqs)
          where freqs = compute' Measuring Empty initialFreq ds

-- | Parse a Drift value.
--
-- >>> parse drift "" "+3"
-- Right (Inc 3)
-- >>> parse drift "" "-6"
-- Right (Dec 6)
-- >>> parse drift "" "+42  "
-- Right (Inc 42)
drift :: Parser Drift
drift = do
    sign <- oneOf "+-"
    num  <- many1 digit
    _    <- spaces
    return $ (if sign == '+' then Inc else Dec) (read num)

-- | Parse a sequence of Drift.
--
-- >>> parse drifts "" "+1, -2, +3, +1"
-- Right [Inc 1,Dec 2,Inc 3,Inc 1]
drifts :: Parser [Drift]
drifts = (sepBy drift sep) <* eof
    where sep = optional (char ',') >> spaces
