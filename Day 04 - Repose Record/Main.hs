module Main (main) where

import Control.Arrow ((&&&))
import Data.Function
import Data.Functor.Identity (Identity)
import Data.List
import Data.Ord
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock
import Data.Time.LocalTime
import Text.Parsec
import Text.ParserCombinators.Parsec
import Text.Printf (printf)

-- $setup
--
-- A Nap constructor accepting timings as string, for testing convenience.
--
-- >>> :{
--     sleep g s s' = Sleeping g (p s) (p s')
--         where p x = either (err x) id (parse utctime "" x)
--               err e _ = error ("Invalid datetime: " ++ e)
-- :}

-- | ID of the guards on duty.
type Guard = Int

-- | A minute count after midnight (0).
type Minute = Int

-- | Record written on the wall.
data Record = Shift  { timing :: UTCTime, gid :: Int }
            | Asleep { timing :: UTCTime }
            | Awake  { timing :: UTCTime }
            deriving (Show)

-- | A time frame during which the guard slept.
data Nap = Sleeping { guard :: Guard, from, to :: UTCTime }
    deriving (Show)

-- | Statistic about a guard's sleep during shift with the total of minutes
-- slept, and the count of the most slept minute.
data Stat = Stat { who :: Guard, total :: Int, quietest :: (Minute, Int) }
    deriving (Show)

-- | The frequency of each elements within a list.
--
-- see Day 02
--
-- >>> freq "bababc"
-- [('a',2),('b',3),('c',1)]
freq :: Ord a => [a] -> [(a, Int)]
freq = map (head &&& length) . group . sort

-- | Whole minutes count in the given number of seconds.
--
-- >>> sec2min 59.99
-- 0
-- >>> sec2min 60
-- 1
-- >>> sec2min 60.01
-- 1
sec2min :: (RealFrac a) => a -> Int
sec2min = floor . (/60)

-- | The sequence of minutes after midnight when the guard was asleep.
--
-- >>> minutes $ sleep 10 "1518-11-01 00:05" "1518-11-01 00:25"
-- [5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24]
-- >>> minutes $ sleep 10 "1518-11-01 00:30" "1518-11-01 00:55"
-- [30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54]
-- >>> minutes $ sleep 99 "1518-11-02 00:40" "1518-11-02 00:50"
-- [40,41,42,43,44,45,46,47,48,49]
-- >>> minutes $ sleep 10 "1518-11-03 00:24" "1518-11-03 00:29"
-- [24,25,26,27,28]
-- >>> minutes $ sleep 99 "1518-11-04 00:36" "1518-11-04 00:46"
-- [36,37,38,39,40,41,42,43,44,45]
-- >>> minutes $ sleep 99 "1518-11-05 00:45" "1518-11-05 00:55"
-- [45,46,47,48,49,50,51,52,53,54]
minutes :: Nap -> [Minute]
minutes n = [x .. (y - 1)]
    where x = sec2min $ utctDayTime (from n)
          y = sec2min $ utctDayTime (to n)

-- | Nap statistic numbers cruncher.
--
-- >>> :{
--     stats [ sleep 10 "1518-11-01 00:05" "1518-11-01 00:25"
--           , sleep 10 "1518-11-01 00:30" "1518-11-01 00:55"
--           , sleep 99 "1518-11-02 00:40" "1518-11-02 00:50"
--           , sleep 10 "1518-11-03 00:24" "1518-11-03 00:29"
--           , sleep 99 "1518-11-04 00:36" "1518-11-04 00:46"
--           , sleep 99 "1518-11-05 00:45" "1518-11-05 00:55"]
-- :}
-- [Stat {who = 10, total = 50, quietest = (24,2)},Stat {who = 99, total = 30, quietest = (45,3)}]
stats :: [Nap] -> [Stat]
stats = map compute . groupByGuard . sortByGuard
    where sortByGuard  = sortBy (comparing guard)
          groupByGuard = groupBy ((==) `on` guard)
          compute xs   = stat (guard $ head xs) (freq $ concatMap minutes xs)
          stat g freqs = Stat g (computeTotal freqs) (findQuietest freqs)
          computeTotal = sum . map snd
          findQuietest = maximumBy (comparing snd)

-- | Strategy 1: the guard that has the most minutes asleep.
--
-- >> strategy1 (Stat 10 50 (24 ,2)) (Stat 99 30 (45, 3))
-- LT
strategy1 :: Stat -> Stat -> Ordering
strategy1 = comparing total

-- | Strategy 2: the guard the most frequently asleep on the same minute.
--
-- >> strategy2 (Stat 10 50 (24 ,2)) (Stat 99 30 (45, 3))
-- LG
strategy2 :: Stat -> Stat -> Ordering
strategy2 = comparing (snd . quietest)

-- | Display the laziest guard info for a given strategy.
answer :: Int -> Stat -> IO ()
answer i Stat {who = g, total = t, quietest = (m, n)} = do
    printf "Strategy %d:" i
    printf " the chosen guard ID is %d (asleep %d minutes in total)" g t
    printf " and the chosen minute is %d (slept %d times):" m n
    printf " %d * %d = %d.\n" g m (g * m)

-- | Find the laziest guard using both strategies.
main :: IO ()
main = do
    input <- getContents
    case parse records "" input of
      Left err -> fail (show err)
      Right rs -> case parse naps "" rs of
          Left err -> fail (show err)
          Right ns -> let ss = stats ns in do
              answer 1 (maximumBy strategy1 ss)
              answer 2 (maximumBy strategy2 ss)

{-|
   Uses Parsec in two phases. The first one parses the input string into a
   sorted sequence of Record. The second pass parses the Stream of Records into
   a sequence of Nap. The parser using our tokens (i.e. Record) was heavily
   inspired by https://www.vex.net/~trebla/haskell/parsec-generally.xhtml
-}

-- | Parse an UTCTime
--
-- Let's assume that the North Pole Manufacturing Lab Timezone is UTC.
--
-- >>> parse utctime "" "1518-11-01 00:30"
-- Right 1518-11-01 00:30:00 UTC
-- >>> parse utctime "" "1518-11-04 00:46"
-- Right 1518-11-04 00:46:00 UTC
-- >>> parse utctime "" "1518-11-01 23:58"
-- Right 1518-11-01 23:58:00 UTC
utctime :: Parser UTCTime
utctime = do
    y  <- count 4 digit
    mo <- char '-' *> count 2 digit
    d  <- char '-' *> count 2 digit
    h  <- char ' ' *> count 2 digit
    mi <- char ':' *> count 2 digit
    let day = fromGregorian (read y) (read mo) (read d)
        tod = TimeOfDay (read h) (read mi) 0
     in return $ localTimeToUTC utc (LocalTime day tod)

-- | Parse a Shift Record without the timing information.
shift :: Parser (UTCTime -> Record)
shift = do
    g <- string "Guard #" *> many1 digit <* string " begins shift"
    return $ \t -> Shift t (read g)

-- | Parse an Asleep Record without the timing information.
asleep :: Parser (UTCTime -> Record)
asleep = do
    _ <- string "falls asleep"
    return Asleep

-- | Parse an Awake Record without the timing information.
awake :: Parser (UTCTime -> Record)
awake = do
    _ <- string "wakes up"
    return Awake

-- | Parse a Record written on the wall.
--
-- >>> parse record "" "[1518-11-01 00:00] Guard #10 begins shift"
-- Right (Shift {timing = 1518-11-01 00:00:00 UTC, gid = 10})
-- >>> parse record "" "[1518-11-01 00:05] falls asleep"
-- Right (Asleep {timing = 1518-11-01 00:05:00 UTC})
-- >>> parse record "" "[1518-11-01 00:25] wakes up"
-- Right (Awake {timing = 1518-11-01 00:25:00 UTC})
record :: Parser Record
record = do
    t   <- char '[' *> utctime <* char ']' <* spaces
    rec <- shift <|> asleep <|> awake
    spaces
    return (rec t)

-- | Parse a sequence of Record and reorder them in a chronological order.
records :: Parser [Record]
records = do
    xs <- many1 record
    eof
    return $ sortBy (comparing timing) xs

-- | Update the source position when parsing Naps from Records.
update :: SourcePos -> Record -> [Record] -> SourcePos
update pos _ [] = pos
update pos _ _  = incSourceColumn pos 1

-- | Consume a Shift Record returning its gid.
begin :: ParsecT [Record] () Identity Guard
begin = tokenPrim show update go where
    go Shift {gid = g} = Just g
    go _ = Nothing

-- | Consume an Asleep Record returning its timing.
falls :: ParsecT [Record] () Identity UTCTime
falls = tokenPrim show update go where
    go Asleep {timing = t} = Just t
    go _ = Nothing

-- | Consume an Awake Record returning its timing.
wakes :: ParsecT [Record] () Identity UTCTime
wakes = tokenPrim show update go where
    go Awake {timing = t} = Just t
    go _ = Nothing

-- | Parse a Nap, i.e. an Asleep followed by an Awake, without the guard
-- information.
nap :: Parsec [Record] () (Guard -> Nap)
nap = do
    t  <- falls
    t' <- wakes
    return $ \g -> Sleeping g t t'

-- | Parse full night shift.
night :: Parsec [Record] () [Nap]
night = do
    g  <- begin
    xs <- many nap
    return $ map ($ g) xs

-- | Parse all the Record found on the wall.
naps :: Parsec [Record] () [Nap]
naps = do
    xs <- many night
    eof
    return $ concat xs
