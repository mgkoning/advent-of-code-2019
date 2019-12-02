module Main where

import Day01
import Day02
import Data.Time.LocalTime (getZonedTime, zonedTimeToLocalTime, localDay)
import Data.Time (utc, localTimeToUTC, addUTCTime, utcToLocalTime, toGregorian)
import Data.Time.Clock (nominalDay)
import System.Environment (getArgs)
import Data.Map.Strict (Map, fromList, lookup)
import Prelude hiding (lookup)

solvers :: Map Int (IO ())
solvers = fromList [(1, Day01.solve), (2, Day02.solve)]

main :: IO ()
main = do
  args <- getArgs
  currentDay <- dayOfMonth <$> puzzleTime
  let dayToRun = case args of
                  [] -> currentDay
                  [x] -> (read x) :: Int
                  _ -> error "I don't understand the arguments"
  putStrLn $ "Running day " ++ (show dayToRun)
  case lookup dayToRun solvers of
    Nothing -> putStrLn $ "Day " ++ (show dayToRun) ++ " not supported"
    Just solve -> solve

dayOfMonth localTime = day
  where (_, _, day) = toGregorian $ localDay $ localTime

puzzleTime = addLocalTime unlockTimeDiff <$> zonedTimeToLocalTime <$> getZonedTime
  where addLocalTime x = utcToLocalTime utc . addUTCTime x . localTimeToUTC utc
        unlockTimeDiff = (-1 * nominalDay / 4)