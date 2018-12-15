import Data.List
import Data.List.Split
import Debug.Trace

data Node = Node {
  children :: [Node],
  metadata :: [Int]
} deriving (Show)

toInt :: String -> Int
toInt = read

createTree :: [Int] -> Node
createTree input = let (tree, _) = parent input in tree

parent :: [Int] -> (Node, [Int])
parent (n:m:xs) = let (c, tail) = leafs n [] xs
                      p = Node c (take m tail)
                      tail' = drop m tail
                  in (p, tail')

leafs :: Int -> [Node] -> [Int] -> ([Node], [Int])
leafs 0 c tail = (c, tail)
leafs x c (0:m:xs) = let c' = c ++ [Node [] (take m xs)]
                         x' = x-1
                         xs' = drop m xs
                     in leafs x' c' xs'
leafs x c (n:m:xs) = let (p, tail) = parent (n:m:xs)
                         x' = x-1
                         c' = c ++ [p]
                     in leafs x' c' tail

countMetadata :: Node -> Int
countMetadata node
  | length childNodes == 0 = sumMeta
  | otherwise = sumMeta + sumChildNodes
  where sumMeta = foldl (+) 0 $ metadata node
        childNodes = children node
        sumChildNodes = foldl (+) 0 $ map countMetadata childNodes

calculateValue :: Node -> Int
calculateValue node
  | length childNodes == 0 = sumMeta
  | otherwise = foldl (+) 0 $ map calculateValue metaChildNodes
  where meta = metadata node
        sumMeta = foldl (+) 0 meta
        childNodes = children node
        metaChildNodes = map (childNodes!!) (filter (length childNodes >) (map (subtract 1) meta))

main = do
  inputRaw <- readFile "input.txt"
  let input = (map toInt) . (splitOn " ") $ inputRaw

  let tree = createTree input
  let part1 = countMetadata tree
  print part1

  let part2 = calculateValue tree
  print part2