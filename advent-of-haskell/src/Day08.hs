module Day08 (solve) where

import Data.List.Split (chunksOf)
import Data.List (minimumBy)
import Data.Ord (comparing)

makeLayers :: [Char] -> Int -> Int -> [[Char]]
makeLayers pixels width height = chunksOf (width * height) pixels

part1 :: [[Char]] -> Int
part1 layers = 
  let count c s = length $ filter (== c) s
      leastZeroes = minimumBy (comparing $ count '0') layers
  in count '1' leastZeroes * count '2' leastZeroes

flattenedImage :: [[Char]] -> [Char]
flattenedImage layers = foldl1 mergeLayers layers
  where mergeLayers :: [Char] -> [Char] -> [Char]
        mergeLayers a b = zipWith mergePixel a b
        mergePixel a b = if a == '2' then b else a

readablePixel '1' = '@'
readablePixel _ = ' '

solve = do
  inputString <- readFile "../input/day08.txt"
  let layers = makeLayers inputString 25 6
  putStrLn "Part 1:"
  print $ part1 layers
  putStrLn "Part 2:"
  putStrLn $ unlines $ chunksOf 25 $ map readablePixel $ flattenedImage layers