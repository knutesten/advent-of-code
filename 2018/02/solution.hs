import Data.List

countSameChar word =
  let wordSorted = sort word
  in countSameChar' (tail wordSorted) (head wordSorted) [1]

countSameChar' word prev counts
  | word == "" = counts
  | head word == prev = countSameChar' (tail word) (head word) ((init counts) ++ [last counts + 1])
  | otherwise = countSameChar' (tail word) (head word) (counts ++ [1])

hasTwo counts = elem 2 counts
hasThree counts = elem 3 counts

hasOnlyOneDifference word1 word2 = hasOnlyOneDifference' word1 word2 0
hasOnlyOneDifference' word1 word2 errors
  | errors > 1 = False
  | word1 == "" = True
  | word2 == "" = True
  | (head word1) == (head word2) = hasOnlyOneDifference' (tail word1) (tail word2) errors
  | otherwise = hasOnlyOneDifference' (tail word1) (tail word2) (errors + 1)

findWordWithOneDifference wordsSorted
  | hasOnlyOneDifference first second = removeDifference first second
  | otherwise = findWordWithOneDifference (tail wordsSorted)
    where first = head wordsSorted
          second = head (tail wordsSorted)

removeDifference word1 word2 = removeDifference' word1 word2 ""
removeDifference' word1 word2 similar
  | word1 == "" = similar
  | head word1 == head word2 = removeDifference' (tail word1) (tail word2) (similar ++ [head word1])
  | otherwise = removeDifference' (tail word1) (tail word2) similar

main = do
  input <- readFile "input.txt"
  let words = lines input

  let counts =  map countSameChar words
  let twos = length (filter hasTwo counts)
  let threes = length (filter hasThree counts)
  let part1 = twos * threes
  print part1

  let wordsSorted = sort words
  let part2 = findWordWithOneDifference wordsSorted
  print part2
