module Main (main) where

import Text.Printf (printf)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token


-- | The device's frequency.
type Freq = Integer

-- | A change in the device's frequency.
data Drift = Inc Freq | Dec Freq
    deriving (Show)


-- | The device's initial frequency.
initialFreq :: Freq
initialFreq = 0


-- | Calibrate the device's frequency with a given change.
--
-- >>> 0 `calibrate1` (Inc 6)
-- 6
-- >>> 6 `calibrate1` (Dec 1)
-- 5
calibrate1 :: Freq -> Drift -> Freq
calibrate1 freq (Inc delta) = freq + delta
calibrate1 freq (Dec delta) = freq - delta


-- | Calibrate the device's frequency with a sequence of changes.
--
-- >>> calibrate "+1, -2, +3, +1"
-- Right 3
-- >>> calibrate "+1, +1, +1"
-- Right 3
-- >>> calibrate "+1, +1, -2"
-- Right 0
-- >>> calibrate "-1, -2, -3"
-- Right (-6)
calibrate :: String -> Either ParseError Freq
calibrate s = case parse parser "" s of
    Left err ->
        Left err
    Right drifts ->
        Right $ foldl (calibrate1) initialFreq drifts


-- | Compute and the calibrated device's frequency.
main :: IO ()
main = do
    input <- getContents
    case calibrate input of
      Left err ->
          print err >> fail "parse error"
      Right freq ->
          print freq


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
-- >>> parse parser "" "+1, -2, +3, +1"
-- Right [Inc 1,Dec 2,Inc 3,Inc 1]
parser :: Parser [Drift]
parser = (sepBy drift sep) <* eof
