module Day15 (solve) where

import Intcode
import Data.Maybe (isNothing, fromJust)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M

move (x, y) direction =
  case direction of
    1 -> (x, y-1) -- North
    2 -> (x, y+1) -- South
    3 -> (x-1, y) -- West
    4 -> (x+1, y) -- East

makeTriple (a, b) c = (a,b,c)
fst3 (a, b, c) = a
snd3 (a, b, c) = b
trd (a, b, c) = c

buildMap program = buildMap' [((0,0), 0, startState)] M.empty Nothing
  where
    startState = State [] program 0 0
    allDirections = [1..4]
    buildMap' :: [((Int, Int), Int, State)] -> HashMap (Int, Int) Int -> Maybe Int -> (HashMap (Int, Int) Int, Maybe Int)
    buildMap' [] visited shortest = (visited, shortest)
    buildMap' ((pos, steps, state):rest) visited shortest =
      let steps' = steps + 1
          possibleMoves = zip3 allDirections (map (move pos) allDirections) (map runToStatus allDirections)
          newMoves = filter (not . (`M.member` visited) . snd3) possibleMoves
          runToStatus n = head $ dropWhile (null . snd) $ doRunProgram state { input = [n] }
          visited' = foldl (\m (_, p, (_, [s])) -> M.insert p s m) visited newMoves
          knownGood = map (\(_, p, (state', _)) -> (p, steps', state')) $ filter ((==1) . head . snd . trd) newMoves
          shortest' = if isNothing shortest && any ((== 2) . head . snd . trd) newMoves
                        then Just steps'
                        else shortest
      in buildMap' (rest ++ knownGood) visited' shortest'

solve = do
  program <- readProgram <$> readFile "../input/day15.txt"
  putStrLn "Part 1:"
  let (builtMap, shortestToOxygenUnit) = buildMap program
  print $ fromJust shortestToOxygenUnit
  let oxygenUnitLocation = fst $ head $ filter ((==2) . snd) $ M.toList builtMap
  print oxygenUnitLocation