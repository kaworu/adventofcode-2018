module Main (main) where

import Data.List
import qualified Data.Map.Strict as M
import Text.ParserCombinators.Parsec
import Text.Printf (printf)

-- | A coordinate in the whole piece of fabric.
type Point = (Int, Int)

-- | The piece of fabric, mapping Point to Claim id sequence.
type Fabric = M.Map Point [Int]

-- | An area of fabric claimed by an Elf.
data Claim = Claim { ident :: Int, tl, br :: Point }
    deriving (Show)

-- | The Elf Claim's points.
points :: Claim -> [Point]
points Claim { tl = (tlx, tly), br = (brx, bry) } =
    [ (x, y) | x <- [tlx..brx], y <- [tly..bry] ]

-- | Draw a Claim on the given Fabric.
--
-- >>> outline (Claim 42 (1, 2) (2, 3)) M.empty
-- fromList [((1,2),[42]),((1,3),[42]),((2,2),[42]),((2,3),[42])]
-- >>> outline (Claim 42 (0, 0) (0, 0)) M.empty
-- fromList [((0,0),[42])]
outline :: Claim -> Fabric -> Fabric
outline cl fab = foldr (M.alter $ add $ ident cl) fab (points cl)
    where add x Nothing   = Just [x]
          add x (Just xs) = Just (x : xs)

-- | The Fabric with only the overlapping points remaining given a sequence of
-- Claim.
--
-- >>> overlap [Claim 1 (1,3) (4,6), Claim 2 (3,1) (6,4), Claim 3 (5,5) (6,6)]
-- fromList [((3,3),[1,2]),((3,4),[1,2]),((4,3),[1,2]),((4,4),[1,2])]
overlap :: [Claim] -> Fabric
overlap = M.filter (\xs -> length xs > 1) . outlineAll
    where outlineAll = foldr outline M.empty

-- | Display the count of square inches within two or more claims and the claim
-- id that doesn't overlap.
answer :: Int -> [Int] -> IO ()
answer inches [] = do
    printf "There are %d square of fabric within two or more claims," inches
    printf " and all claims overlap.\n"
answer inches [intact] = do
    printf "There are %d square of fabric within two or more claims," inches
    printf " and the claim %d doesn't overlap.\n" intact
answer inches overlapping = do
    printf "There are %d square of fabric within two or more claims," inches
    printf " and many claims don't overlap: %s\n" (show overlapping)

-- | Compute and display the count of square inches within two or more claims.
main :: IO ()
main = do
    input <- getContents
    case parse claims "" input of
      Left err -> error (show err)
      Right xs -> answer (length pts) (map ident xs \\ ids)
          where fab = overlap xs -- overlapping fabric
                pts = M.keys fab -- overlapping points
                ids = nub $ concat $ M.elems fab -- overlapping identities

-- | Parse an Elf Claim.
--
-- >>> parse claim "" "#123 @ 3,2: 5x4"
-- Right (Claim {ident = 123, tl = (3,2), br = (7,5)})
claim :: Parser Claim
claim = do
    i      <- char '#' >> many1 digit
    left   <- spaces >> char '@' >> spaces >> many1 digit
    top    <- spaces >> char ',' >> spaces >> many1 digit
    width  <- spaces >> char ':' >> spaces >> many1 digit
    height <- spaces >> char 'x' >> spaces >> many1 digit
    spaces
    let tlx = read left
        tly = read top
        brx = read width + tlx - 1
        bry = read height + tly - 1
     in return $ Claim (read i) (tlx, tly) (brx, bry)

-- | Parse a sequence of Elf Claim.
--
-- >>> parse claims "" "#1 @ 1,3: 4x4 #2 @ 3,1: 4x4 #3 @ 5,5: 2x2"
-- Right [Claim {ident = 1, tl = (1,3), br = (4,6)},Claim {ident = 2, tl = (3,1), br = (6,4)},Claim {ident = 3, tl = (5,5), br = (6,6)}]
claims :: Parser [Claim]
claims = sepBy1 claim spaces <* eof
