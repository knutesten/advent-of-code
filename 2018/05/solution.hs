import Data.List
import Data.Char
import qualified Data.Text as T

removePairs input =
  let
    chars = sort . nub . (map toLower) $ input
    inputText = T.pack input
    pairsToRemove = map T.pack (concat [[a:toUpper a:[], toUpper a:a:[]] | a  <- chars])
  in T.length $ removePairs' T.empty inputText pairsToRemove

removePairs' old new pairsToRemove
  | old == new = new
  | otherwise = removePairs' new (react new) pairsToRemove
  where react str = foldl (\acc pair -> T.replace pair T.empty acc) str pairsToRemove

removeChar :: [Char] -> Char -> [Char]
removeChar str c = filter (\x -> (toLower x) /= c) str

main = do
  input <- readFile "input.txt"

  let part1 = removePairs input
  print part1

  let chars = sort . nub . (map toLower) $ input
  let part2 = minimum $ map removePairs $ map (removeChar input) chars
  print part2
