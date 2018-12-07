import Data.List
import qualified Data.Set as Set

removePlus str
  | head str == '+' = tail str
  | otherwise = str

findDuplicateFrequency freqChanges = findDuplicateFrequency' freqChanges 0 0 Set.empty
findDuplicateFrequency' freqChanges index currFreq prevFreqs
  | Set.member currFreq prevFreqs = currFreq
  | otherwise = findDuplicateFrequency' freqChanges newIndex newCurrFreq newPrevFreqs
    where
          newIndex = index + 1
          newCurrFreq = currFreq + freqChanges !! (mod index (length freqChanges))
          newPrevFreqs = Set.insert currFreq prevFreqs

main = do
  input <- readFile "input.txt"
  let freqChanges = map ((read :: String -> Int) . removePlus) (lines input)
  let part1 = sum freqChanges
  print part1
  let part2 = findDuplicateFrequency freqChanges
  print part2


