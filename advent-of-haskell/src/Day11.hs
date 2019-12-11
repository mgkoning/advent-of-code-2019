{-# LANGUAGE RecordWildCards #-}

module Day11 (solve) where

import Prelude hiding (Left, Right)
import Intcode
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M

type Coord = (Int, Int)
type Color = Int

data RobotState = RobotState { input :: [Int], position :: Coord, direction :: Direction, paintedPanels :: HashMap Coord Int }
data Direction = Up | Down | Left | Right
data MoveInstruction = TurnLeft | TurnRight

getPos pos@(x,y) dir turn =
  let newDirection =
        case (dir, turn) of
          (Up, TurnLeft) -> Left
          (Left, TurnLeft) -> Down
          (Down, TurnLeft) -> Right
          (Right, TurnLeft) -> Up
          (Up, TurnRight) -> Right
          (Right, TurnRight) -> Down
          (Down, TurnRight) -> Left
          (Left, TurnRight) -> Up
      newPosition = case newDirection of
        Up -> (x, y+1)
        Left -> (x-1, y)
        Down -> (x, y-1)
        Right -> (x+1, y)
  in (newPosition, newDirection)

runRobot :: RobotState -> [(RobotState, [Color])]
runRobot s@RobotState{..} =
  case input of
    [] -> []
    _:[] -> []
    (color:turn:inputs) ->
      let newState = RobotState inputs newPos newDir newPaint
          (newPos, newDir) = getPos position direction (getTurn turn)
          newPaint = M.insert position color paintedPanels
          pixelColor = M.lookupDefault 0 newPos newPaint
      in (newState, [pixelColor]):(runRobot newState)
  where getTurn turn = case turn of 0 -> TurnLeft
                                    1 -> TurnRight

robot brain initial =
  let instruction = runProgram brain robotOutputs
      state = RobotState instruction (0,0) Up M.empty
      robotStates = (state, [initial]):(runRobot state)
      robotOutputs = foldr (++) [] $ map snd robotStates
      (lastState, _) = last robotStates
  in paintedPanels lastState

getPicture :: HashMap Coord Int -> [[Char]]
getPicture paintedPanels =
  let coords = M.keys paintedPanels
      maxX = maximum $ map fst coords
      minX = minimum $ map fst coords
      maxY = maximum $ map snd coords
      minY = minimum $ map snd coords
      toPixel c = if c == 0 then ' ' else '@'
  in map (\y -> map (\x -> toPixel $ M.lookupDefault 0 (x, y) paintedPanels) [minX,minX+1..maxX]) [maxY,maxY-1..minY]

solve = do
  brain <- readProgram <$> readFile "../input/day11.txt"
  putStrLn "Part 1:"
  let part1 = robot brain 0
  print $ length $ M.keys part1
  putStrLn "Part 2:"
  let part2 = robot brain 1
      picture = getPicture part2
  putStrLn $ unlines picture