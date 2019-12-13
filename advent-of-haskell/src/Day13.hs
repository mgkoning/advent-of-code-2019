module Day13 (solve) where

import Intcode
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.List.Split (chunksOf)
import Text.Printf

readOutputTriple [x,y,t] = ((x,y), t)
readOutputTriple _ = error "not three outputs"

toScreen output =
  let outputs = map readOutputTriple $ chunksOf 3 output
      drawnTiles = foldr (\next@(coord, tile) map -> M.insert coord tile map) M.empty outputs
  in drawnTiles

paintScreen grid =
  let keys = M.keys grid
      maxX = maximum $ map fst keys
      maxY = maximum $ map snd keys
      score = printf "[Score]%33d\n" (M.lookupDefault 0 (-1,0) grid)
      tiles = unlines $ [[getTile (M.lookupDefault 0 (x,y) grid) | x <- [0..maxX]] | y <- [0..maxY]]
  in score ++ tiles

getTile t = case t of
              0 -> ' '
              1 -> '#'
              2 -> '-'
              3 -> '_'
              4 -> '@'

solve = do
  program <- readProgram <$> readFile "../input/day13.txt"
  putStrLn "Part 1:"
  let screen = toScreen $ runProgram program []
  putStrLn $ paintScreen screen
  print $ length $ M.keys $ M.filter (==2) screen
  putStrLn "Part 2:"
  --let runner = runProgram (memSet program 0 2) <$> (sequence $ repeat $ fmap toInput getChar)
  --print "x"
  --mapM_ putStrLn
  --print $ runProgram (memSet program 0 2) []

toInput c
  | c == 'a' = -1
  | c == 's' = 0
  | c == 'd' = 1