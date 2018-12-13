import Data.List
import qualified Data.Map as M
import Data.Char
import Data.Ord

parseInput :: [Char] -> [(Char, Char)]
parseInput input =
  let instList = map (\x -> (x!!5, x!!36)) $ lines input
  in instList

findGoal input =
  let instList =  parseInput input
      steps = getSteps instList
  in findGoal' steps instList ""

findGoal' steps instList solution
  | steps == "" = solution
  | otherwise = findGoal' stepsLeft instLeft (solution ++ [nextStep])
  where locked = map snd instList
        nextStep = (steps \\ locked)!!0
        stepsLeft = delete nextStep steps
        instLeft = filter (\(a,_) -> a /= nextStep) instList

getAmountOfWork :: Char -> Int
getAmountOfWork c = ord c - 4

getSteps = sort . nub . concat . (map (\(a,b) -> [a,b]))

timeToFinishAllSteps input =
  let instList =  parseInput input
      stepsWithTime = foldl (\acc x -> M.insert x (getAmountOfWork x) acc) M.empty $ getSteps instList
  in timeToFinishAllSteps' stepsWithTime instList 5 0

timeToFinishAllSteps' stepsWithTime instList workers time
  | stepsWithTime == M.empty = time
  | otherwise = timeToFinishAllSteps' stepsLeftWithTime instLeft workers (time + 1)
  where locked = map snd instList
        nonLockedSteps = M.toList $ M.filterWithKey (\x _ -> not $ elem x locked) stepsWithTime
        availableSteps = map fst $ sortBy (flip $ comparing snd) nonLockedSteps
        nextSteps = take workers availableSteps
        newStepsWithTime = foldl (\acc x -> M.insertWith (+) x (-1) acc) stepsWithTime nextSteps
        stepsLeftWithTime = M.filter (> 0) newStepsWithTime
        finishedSteps = M.keys $ M.filter (== 0) newStepsWithTime
        instLeft = filter (\(a,_) -> not $ elem a finishedSteps) instList

main = do
  input <- readFile "input.txt"

  let part1 = findGoal input
  print part1

  let part2 = timeToFinishAllSteps input
  print part2

