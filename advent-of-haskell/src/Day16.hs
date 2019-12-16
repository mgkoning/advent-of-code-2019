module Day16 (solve, test) where

import Data.Char (digitToInt, intToDigit)
import Data.List (iterate', foldl')

parseSignal s = map digitToInt s

getMatrix :: [Int] -> [[Int]]
getMatrix s =
  let digitPatterns = map makePattern [1..]
      signalLength = length s
      makePattern i = take signalLength $ drop 1 $ cycle $ concatMap (replicate i) [0,1,0,-1]
  in take signalLength digitPatterns

applyFftPhase :: [[Int]] -> [Int] -> [Int]
applyFftPhase matrix s = map (((`mod` 10) . abs) . sum . (zipWith (*) s)) matrix

fft s =
  let matrix = getMatrix s
  in map intToDigit $ head $ drop 100 $ iterate (applyFftPhase matrix) s

fft2 s = reverse $ fst $ foldl' accum ([], 0) s
  where accum (l, s) d = let d' = (s + d) `mod` 10 in d' `seq` (d':l, d')

solve = do
  input <- readFile "../input/day16.txt"
  let signal = parseSignal input
  putStrLn "Part 1:"
  putStrLn $ take 8 $ fft signal
  putStrLn "Part 2:"
  let offset = (read (take 7 input)) :: Int
  let input2 = concat $ replicate 10000 signal
  let partToCalculate = reverse $ drop offset input2
  putStrLn $ map intToDigit $ take 8 $ reverse $ head $ drop 100 $ iterate' fft2 $ partToCalculate

test = do
  print $ take 5 $ iterate (applyFftPhase (getMatrix [1,2,3,4,5,6,7,8])) [1,2,3,4,5,6,7,8]
  putStrLn $ take 8 $ fft $ parseSignal "80871224585914546619083218645595"
  putStrLn $ take 8 $ fft $ parseSignal "19617804207202209144916044189917"
  putStrLn $ take 8 $ fft $ parseSignal "69317163492948606335995924319873"

