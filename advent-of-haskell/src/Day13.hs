module Day13 (solve) where

import Intcode
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.List.Split (chunksOf)
import Text.Printf (printf)
import Printing (printGrid)

readOutputTriple [x,y,t] = ((x,y), t)
readOutputTriple _ = error "not three outputs"

toScreen :: [Int] -> HashMap (Int, Int) Int
toScreen output =
  let outputs = map readOutputTriple $ chunksOf 3 output
      drawnTiles = foldl (\ map next@(coord, tile) -> M.insert coord tile map) M.empty outputs
  in drawnTiles

paintScreen :: HashMap (Int, Int) Int -> String
paintScreen grid =
  let score = printf "[Score]%33d\n" (M.lookupDefault 0 (-1,0) grid)
      tiles = printGrid (M.delete (-1, 0) grid) getTile 0
  in score ++ tiles

getTile t = case t of
              0 -> ' '
              1 -> '#'
              2 -> '-'
              3 -> '_'
              4 -> '@'

getCommands output paddlePos =
  let (paddle, ball, output') = consume' paddlePos Nothing output
      consume' (Just p) (Just b) o = (p, b, o)
      consume' p b (x:y:t:os) =
          case t of 4 -> consume' p (Just x) os
                    3 -> consume' (Just x) b os
                    _ -> consume' p b os
      determineMove px bx
        | bx < px = -1
        | bx == px = 0
        | px < bx = 1
      move = determineMove paddle ball
  in {- We have to keep track of the paddle in case we don't move it -}
      move:(getCommands output' (if move == 0 then Just paddle else Nothing))

finalScreen program =
  let noQuarters = runProgram (memSet program 0 2) joystickCommands
      joystickCommands = getCommands noQuarters Nothing
  in toScreen noQuarters

solve :: IO ()
solve = do
  program <- readProgram <$> readFile "../input/day13.txt"
  putStrLn "Part 1:"
  let screen = toScreen $ runProgram program []
  putStrLn $ paintScreen screen
  print $ length $ M.keys $ M.filter (==2) screen
  putStrLn "Part 2:"
  let final = finalScreen program
  putStrLn $ paintScreen final
  print $ final M.! (-1, 0)
