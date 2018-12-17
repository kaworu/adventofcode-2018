module Main (main) where

import qualified Data.Map.Strict as M
import Text.Printf (printf)

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

-- | A coordinate in the whole piece of fabric.
type Point = (Int, Int)

-- | The piece of fabric.
type Fabric = M.Map Point Int

-- | An area of fabric claimed by an Elf.
data Claim = Claim
    { ident :: Int
    , tl :: Point
    , br :: Point
    } deriving (Show)

-- | The Elf Claim's points.
points :: Claim -> [Point]
points Claim { tl = (tlx, tly), br = (brx, bry) } =
    [ (x, y) | x <- [tlx..brx], y <- [tly..bry] ]

-- | Draw a Claim on the given Fabric.
--
-- >>> outline M.empty $ Claim 42 (1, 2) (2, 3)
-- fromList [((1,2),1),((1,3),1),((2,2),1),((2,3),1)]
-- >>> outline M.empty $ Claim 42 (0, 0) (0, 0)
-- fromList [((0,0),1)]
outline :: Fabric -> Claim -> Fabric
outline fab = foldr (M.alter inc) fab . points
    where inc Nothing  = Just 1
          inc (Just x) = Just (x + 1)

-- | The Fabric with only the overlapping points remaining given a sequence of
-- Claim.
--
-- >>> overlap [Claim 42 (1,3) (4,6), Claim 43 (3,1) (6,4), Claim 43 (5,5) (6,6)]
-- fromList [((3,3),2),((3,4),2),((4,3),2),((4,4),2)]
overlap :: [Claim] -> Fabric
overlap = M.filter (> 1) . outlineAll
    where outlineAll = foldl (outline) M.empty

-- | Display the count of square inches within two or more claims.
-- correct box IDs.
answer :: Int -> IO ()
answer = do
    printf "There are %d square inches of fabric within two or more claims.\n"

-- | Compute and display the count of square inches within two or more claims.
main :: IO ()
main = do
    input <- getContents
    case parse claims "" input of
      Left err -> print err >> fail "parse error"
      Right xs -> answer (M.size $ overlap xs)

-- | Parse an Elf Claim.
--
-- >>> parse claim "" "#123 @ 3,2: 5x4"
-- Right (Claim {ident = 123, tl = (3,2), br = (7,5)})
claim :: Parser Claim
claim = do
    ident  <- char '#' >> many1 digit
    left   <- spaces >> char '@' >> spaces >> many1 digit
    top    <- spaces >> char ',' >> spaces >> many1 digit
    width  <- spaces >> char ':' >> spaces >> many1 digit
    height <- spaces >> char 'x' >> spaces >> many1 digit
    _ <- spaces
    let tlx = read left
        tly = read top
        brx = (read width) + tlx - 1
        bry = (read height) + tly - 1
     in return $ Claim (read ident) (tlx, tly) (brx, bry)

-- | Parse a sequence Elf Claim.
--
-- >>> parse claims "" "#1 @ 1,3: 4x4 #2 @ 3,1: 4x4 #3 @ 5,5: 2x2"
-- Right [Claim {ident = 1, tl = (1,3), br = (4,6)},Claim {ident = 2, tl = (3,1), br = (6,4)},Claim {ident = 3, tl = (5,5), br = (6,6)}]

claims :: Parser [Claim]
claims = (sepBy1 claim spaces) <* eof
