module Main where

import System.Environment (getArgs)
import Today (currentDayOfMonth)
import Solvers (getSolver)

main :: IO ()
main = do
  args <- getArgs
  currentDay <- currentDayOfMonth
  let dayToRun =
        case args of
          [] -> currentDay
          [x] -> (read x) :: Int
          _ -> error "I don't understand the arguments"
  putStrLn $ "Running day " ++ (show dayToRun)
  case getSolver dayToRun of
    Nothing -> putStrLn $ "Day " ++ (show dayToRun) ++ " not supported"
    Just solve -> solve
