module Main (main) where

import Data.Set
import Text.Printf (printf)

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

-- | Device's frequency.
type Freq = Integer

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
search (Searching s) f | f `member` s = Found f
                       | otherwise = Searching (f `insert` s)
search Empty f = Searching (singleton f)

-- | Given a initial frequency, a complete list of changes, and a rest of list
-- of changes, measure the resulting frequency after all the changes have been
-- applied and find the first frequency reached twice.
compute :: FinalFreq -> TwiceFreq -> Freq -> [Drift] -> [Drift] -> (Freq, Freq)
compute (Measured ff) (Found t2) _ _ _ = (ff, t2)
compute fin twice f ds [] = compute (fin `measure` f) twice f ds ds
compute fin twice f ds (x:xs) = compute fin (search twice f) (f `calibrate` x) ds xs

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
part1 input = let part1' = fst . compute' Measuring (Found 0) initialFreq
           in either Left (Right . part1') (parse drifts "" input)

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
part2 input = let part2' = snd . compute' (Measured 0) Empty initialFreq
           in either Left (Right . part2') (parse drifts "" input)

-- | Compute and the calibrated device's frequency.
main :: IO ()
main = do
    input <- getContents
    case parse drifts "" input of
      Left err ->
          print err >> fail "parse error"
      Right ds -> let freqs = compute' Measuring Empty initialFreq ds
                   in printf "The resulting frequency is %d and the first frequency reached twice is %d.\n" (fst freqs) (snd freqs)

-- | Parse an Freq value.
--
-- >>> parse frequency "" "42"
-- Right 42
frequency :: Parser Freq
frequency = Token.integer (Token.makeTokenParser emptyDef)

-- | Parse a Drift value.
--
-- >>> parse drift "" "+3"
-- Right (Inc 3)
-- >>> parse drift "" "-6"
-- Right (Dec 6)
drift :: Parser Drift
drift = do
    sign <- oneOf "+-"
    freq <- frequency
    return $ (if sign == '+' then Inc else Dec) freq

-- | Parse a Drift separator.
-- Accept either the examples from the README or the input format.
sep :: Parser ()
sep = (optional $ char ',') >> spaces

-- | Parse a sequence of Drift.
--
-- >>> parse drifts "" "+1, -2, +3, +1"
-- Right [Inc 1,Dec 2,Inc 3,Inc 1]
drifts :: Parser [Drift]
drifts = (sepBy drift sep) <* eof
