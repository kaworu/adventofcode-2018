module Main (main) where

import Data.Function
import Data.Functor.Identity (Identity)
import Data.List
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

-- | Record written on the wall.
data Record = Shift  { timing :: UTCTime, gid :: Int }
            | Asleep { timing :: UTCTime }
            | Awake  { timing :: UTCTime }
            deriving (Show)

-- | A time frame during which the guard slept.
data Nap = Sleeping { guard :: Guard, from, to :: UTCTime }
    deriving (Show)

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

-- | Count of whole minute slept.
--
-- >>> duration $ sleep 10 "1518-11-01 00:05" "1518-11-01 00:25"
-- 20
-- >>> duration $ sleep 10 "1518-11-01 00:30" "1518-11-01 00:55"
-- 25
-- >>> duration $ sleep 99 "1518-11-02 00:40" "1518-11-02 00:50"
-- 10
-- >>> duration $ sleep 10 "1518-11-03 00:24" "1518-11-03 00:29"
-- 5
-- >>> duration $ sleep 99 "1518-11-04 00:36" "1518-11-04 00:46"
--10
-- >>> duration $ sleep 99 "1518-11-05 00:45" "1518-11-05 00:55"
--10
duration :: Nap -> Int
duration n = sec2min $ diffUTCTime (to n) (from n)

-- | The guard having spent the most time sleep.
--
-- >>> :{
--     laziest [ sleep 10 "1518-11-01 00:05" "1518-11-01 00:25"
--             , sleep 10 "1518-11-01 00:30" "1518-11-01 00:55"
--             , sleep 99 "1518-11-02 00:40" "1518-11-02 00:50"
--             , sleep 10 "1518-11-03 00:24" "1518-11-03 00:29"
--             , sleep 99 "1518-11-04 00:36" "1518-11-04 00:46"
--             , sleep 99 "1518-11-05 00:45" "1518-11-05 00:55"]
-- :}
-- 10
laziest :: [Nap] -> Guard
laziest = sleptTheMost . groupByGuard . sortByGuard
    where sortByGuard  = sortBy (compare `on` guard)
          groupByGuard = groupBy ((==) `on` guard)
          sleptTheMost = guard . head . maximumBy (compare `on` totalNapTime)
          totalNapTime = sum . map duration

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
minutes :: Nap -> [Int]
minutes n = [x .. (y - 1)]
    where x = sec2min $ utctDayTime (from n)
          y = sec2min $ utctDayTime (to n)

-- | The most slept minute after midnight.
--
-- >>> :{
--     quietest [ sleep 10 "1518-11-01 00:05" "1518-11-01 00:25"
--              , sleep 10 "1518-11-01 00:30" "1518-11-01 00:55"
--              , sleep 10 "1518-11-03 00:24" "1518-11-03 00:29"]
-- :}
-- 24
quietest :: [Nap] -> Int
quietest = longest . group . sort . concatMap minutes
    where longest = head . maximumBy (compare `on` length)

-- | Display the guard that has the most minutes asleep, and the minute spend
-- asleep the most.
answer :: (Guard, Int) -> IO ()
answer (g, m) = do
    printf "The chosen guard ID is %d" g
    printf " the chosen minute is %d" m
    printf " (%d * %d = %d).\n" g m (g * m)

-- | Find the guard that has the most minutes asleep, and the minute spend
-- asleep the most.
main :: IO ()
main = do
    input <- getContents
    case parse records "" input of
      Left err -> error (show err)
      Right xs -> case parse naps "" xs of
          Left err -> error (show err)
          Right ns ->
              let sloth = laziest ns
                  ns' = filter (\n -> guard n == sloth) ns
               in answer (sloth, quietest ns')

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
    xs <- sepBy1 record spaces
    eof
    return (unshuffle xs)
        where unshuffle = sortBy (compare `on` timing)

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

-- | Parse a Nap, i.e. a sequence of [Asleep, Awake], without the guard
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
