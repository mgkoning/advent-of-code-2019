module Day19 (solve) where

import Intcode
import Printing
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M

toChar 0 = '.'
toChar 1 = '@'

solve = do
  droneProgram <- readProgram <$> readFile "../input/day19.txt"
  putStrLn "Part 1:"
  let droneReports = M.fromList [((x, y), head $ runProgram droneProgram [x, y]) | x <- [0..49], y <- [0..49]]
  putStrLn $ printGrid droneReports toChar 0
  print $ M.foldl' (+) 0 droneReports

  let getDrone x y = head $ runProgram droneProgram [x, y]
  let getReport p@(x, y) =
        (,) p (getDrone x y, getDrone (x + 99) y, getDrone (x + 100) y, getDrone x (y + 99),  getDrone x (y + 100))
  {- Approximated on paper, do a search for the specific point we need -}
  let candidates = filter ((==(1,1,0,1,0)) . snd) $ map getReport [(x, y) | x <- [1300..1344], y <- [1000..1078]]
  print $ minimum $ map (\(x,y) -> x*10000 + y) $ map fst candidates
