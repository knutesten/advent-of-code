import qualified Data.Map as M
import qualified Data.Sequence as S

play board current marble max players scores
  | marble > max         = scores
  | (mod marble 23) == 0 = play board'' current'' marble' max players scores''
  | otherwise            = play board' current' marble' max players scores
  where marble' = marble + 1
        current' = mod (current + 2) (S.length board)
        board' = S.insertAt current' marble board

        current'' = mod (current - 7) (S.length board)
        board'' = S.deleteAt current'' board

        player = (mod (marble - 1) players) + 1
        scores' = M.insertWith (+) player marble scores
        scores'' = M.insertWith (+) player (board `S.index` current'') scores'

main = do
  let scores = play (S.fromList [0]) 0 1 71920 403 M.empty
  let part1 = maximum $ M.elems scores
  print part1

  let scores = play (S.fromList [0]) 0 1 7192000 403 M.empty
  let part2 = maximum $ M.elems scores
  print part2

