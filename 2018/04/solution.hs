import Data.List
import Data.List.Split
import Data.Ord
import qualified Data.Map as M

data Event id = Asleep | Wakes | BeginShift id deriving (Show, Eq)

data Record = Record {
  minute :: Int
, event  :: Event Int
} deriving (Show)

getId :: Event Int -> Int
getId shift = let BeginShift id = shift in id

toInt :: [Char] -> Int
toInt str = read str

parseEvent :: [Char] -> Event Int
parseEvent string
  | isInfixOf "asleep" string = Asleep
  | isInfixOf "wakes" string = Wakes
  | otherwise = BeginShift (toInt ((splitOn " " ((splitOn "#" string)!!1))!!0))

parseRecord :: [Char] -> Record
parseRecord string =
  let
    (_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:m1:m2:_) = string
    minute = toInt (m1:m2:[])
  in Record minute (parseEvent string)

calculateGuardsSleep records = calculateGuardsSleep' records 0 0 M.empty
calculateGuardsSleep' records currentGuard startSleep guards
  | (length records) == 0 = guards
  | e == Asleep = calculateGuardsSleep' (tail records) currentGuard m guards
  | e == Wakes = calculateGuardsSleep' (tail records) currentGuard m (M.insertWith (++) currentGuard d guards)
  | otherwise = calculateGuardsSleep' (tail records) g m (M.insert g (M.findWithDefault [] g guards) guards)
  where
    r = head records
    e = event r
    m = minute r
    d = [startSleep..(m-1)]
    g = getId e

mostCommonValue :: Ord a => [a] -> a
mostCommonValue = head . head . sortBy (flip $ comparing length) . group . sort

mostCommonValueCount :: Ord a => [a] -> Int
mostCommonValueCount [] = 0
mostCommonValueCount lst = length . head . sortBy (flip $ comparing length) . group . sort $ lst

calcPart guards strategy =
  let id = fst . (maximumBy $ comparing snd) . (map $ \x -> (fst x, strategy $ snd x)) . M.assocs $ guards
      com = mostCommonValue $ guards M.! id
  in (id * com)

calcPart1 guards = calcPart guards length
calcPart2 guards = calcPart guards mostCommonValueCount

main = do
  input <- readFile "input.txt"
  let inputLines = lines input
  let inputLinesSorted = sort inputLines
  let records = map parseRecord inputLinesSorted

  let guards = calculateGuardsSleep records
  let part1 = calcPart1 guards
  print part1

  let part2 = calcPart2 guards
  print part2
