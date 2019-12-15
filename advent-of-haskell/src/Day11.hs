{-# LANGUAGE RecordWildCards #-}

module Day11 (solve) where

import Prelude hiding (Left, Right)
import Intcode
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Printing (printGrid)

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
        Up -> (x, y-1)
        Left -> (x-1, y)
        Down -> (x, y+1)
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

getPicture :: HashMap Coord Int -> String
getPicture paintedPanels =
  let toPixel c = if c == 0 then ' ' else '#'
  in printGrid paintedPanels toPixel 0

solve = do
  brain <- readProgram <$> readFile "../input/day11.txt"
  putStrLn "Part 1:"
  let part1 = robot brain 0
  putStrLn $ getPicture part1
  print $ length $ M.keys part1
  putStrLn "Part 2:"
  let part2 = robot brain 1
  putStrLn $ getPicture part2