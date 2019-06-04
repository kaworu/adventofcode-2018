module Main (main) where

import Text.ParserCombinators.Parsec
import Text.Printf (printf)

-- | A node from the system's license file's tree.
data Node = Node [Node] [Int]
    deriving (Show)

-- | The sum of all the metadata entries.
--
-- >>> checksum $ Node [] []
-- 0
-- >>> checksum $ Node [Node [] [10,11,12],Node [Node [] [99]] [2]] [1,1,2]
-- 138
checksum :: Node -> Int
checksum (Node cs ms) = sum ms + sum (map checksum cs)

-- | The value of a Node.
--
-- >>> value $ Node [] []
-- 0
-- >>> value $ Node [Node [] [10,11,12],Node [Node [] [99]] [2]] [1,1,2]
-- 66
value :: Node -> Int
value (Node [] ms) = sum ms
value (Node cs ms) = sum $ map (value . (cs!!) . pred) $ filter refer ms
    where refer i = i > 0 && i <= length cs

-- | Display the sum of all metadata entries and the value of the root node.
answer :: Int -> Int -> IO ()
answer s v = do
    printf "The sum of all metadata entries is %d," s
    printf " and the value of the root node is %d.\n" v

-- | Compute and display the sum of all metadata entries and the value of the
-- root node.
main :: IO ()
main = do
    input <- getContents
    case parse tree "" input of
      Left err -> fail (show err)
      Right t -> answer (checksum t) (value t)

-- | Parse a positive integer.
--
-- >>> parse number "" "0"
-- Right 0
-- >>> parse number "" "42"
-- Right 42
-- >>> parse number "" "-1"
-- Left (line 1, column 1):
-- unexpected "-"
-- expecting white space or digit
number :: Parser Int
number = do
    x <- spaces >> many1 digit
    return (read x)

-- | Parse the system's license file into a tree.
--
-- >>> parse tree "" "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
-- Right (Node [Node [] [10,11,12],Node [Node [] [99]] [2]] [1,1,2])
tree :: Parser Node
tree = do
    c  <- number
    m  <- number
    cs <- count c tree
    ms <- count m number
    return (Node cs ms)
