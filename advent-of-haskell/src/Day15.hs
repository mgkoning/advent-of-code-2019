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

buildMap program = buildMap' [((0,0), 0, startState)] M.empty Nothing
  where
    startState = State [] program 0 0
    allDirections = [1..4]
    buildMap' :: [((Int, Int), Int, State)] -> HashMap (Int, Int) Int -> Maybe Int -> (HashMap (Int, Int) Int, Maybe Int)
    buildMap' [] visited shortest = (visited, shortest)
    buildMap' ((pos, steps, state):rest) visited shortest =
      let steps' = steps + 1
          possibleMoves = zip (map (move pos) allDirections) (map runToStatus allDirections)
          newMoves = filter (not . (`M.member` visited) . fst) possibleMoves
          runToStatus n = head $ dropWhile (null . snd) $ doRunProgram state { input = [n] }
          visited' = foldl (\m (p, (_, [s])) -> M.insert p s m) visited newMoves
          knownGood = map (\(p, (state', _)) -> (p, steps', state')) $ filter ((==1) . head . snd . snd) newMoves
          shortest' = if any ((== 2) . head . snd . snd) newMoves then Just steps' else shortest
      in buildMap' (rest ++ knownGood) visited' shortest'

fillWithOxygen :: HashMap (Int, Int) Int -> (Int, Int) -> Int
fillWithOxygen areaMap startAt = fillWithOxygen' [(startAt, 0)] M.empty
  where
    allDirections = [1..4]
    fillWithOxygen' [] visited = maximum $ M.elems visited
    fillWithOxygen' ((pos, steps):rest) visited =
      let allMoves = map (move pos) allDirections
          newMoves = filter (not . (`M.member` visited)) allMoves
          legalMoves = filter ((==1) . (areaMap M.!)) newMoves
          steps' = steps + 1
          visited' = foldl (\m p -> M.insert p steps' m) visited legalMoves
      in fillWithOxygen' (rest ++ (zip legalMoves (repeat steps'))) visited'

solve = do
  program <- readProgram <$> readFile "../input/day15.txt"
  putStrLn "Part 1:"
  let (builtMap, shortestToOxygenUnit) = buildMap program
  print $ fromJust shortestToOxygenUnit
  let oxygenUnitLocation = fst $ head $ filter ((==2) . snd) $ M.toList builtMap
  putStrLn "\nThe map:"
  putStrLn $ printMap builtMap
  putStrLn "Part 2:"
  print $ fillWithOxygen builtMap oxygenUnitLocation

{- Just for fun -}
printMap grid =   
  let keys = M.keys grid
      minX = minimum $ map fst keys
      maxX = maximum $ map fst keys
      minY = minimum $ map snd keys
      maxY = maximum $ map snd keys
      getTile t = case t of 0 -> '#'
                            1 -> '.'
                            2 -> '@'
      tiles = unlines $ [[getTile (M.lookupDefault 0 (x,y) grid) | x <- [minX..maxX]] | y <- [minY..maxY]]
  in tiles