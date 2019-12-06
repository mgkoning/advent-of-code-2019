module Day06 (solve, test2) where

import Parsing
import Data.List ((\\))
import Text.Parsec (sepBy, string, alphaNum, many1)
import Text.Parsec.String (Parser)
import qualified Data.HashMap.Strict as H

data Orbit = Orbit String String deriving (Eq, Show)

parseMap s = parseLines parseOrbit s
  where parseOrbit = Orbit <$> parseBody <* string ")" <*> parseBody
        parseBody = many1 alphaNum

buildOrbitHashMap orbits = H.fromListWith (++) (map toTuple orbits)
  where toTuple orbit@(Orbit a b) = (a, [orbit])

countOrbits orbits =
  let hashMap = buildOrbitHashMap orbits
      orbitCountMap = fst $ head $ dropWhile ((/=[]) . snd) $ iterate addOne (H.fromList [("COM", 0)], hashMap H.! "COM")
      addOne (map, (Orbit a b):os) = (H.insert b (1 + map H.! a) map, os ++ (maybe [] id $ H.lookup b hashMap))
  in H.foldl' (+) 0 orbitCountMap

findMinimumTraversals orbits start destination =
  let moveUpMap = H.fromList $ map (\(Orbit a b) -> (b, a)) orbits
      pathToRoot from soFar = case H.lookup from moveUpMap of
                                Nothing -> soFar
                                Just x -> pathToRoot x (from:soFar)
      startPath = pathToRoot start []
      destinationPath = pathToRoot destination []
      dropCommonPrefix [] bs = bs
      dropCommonPrefix as [] = as
      dropCommonPrefix (a:as) (b:bs) = if a == b then dropCommonPrefix as bs else (a:as) ++ (b:bs)
      minPath = dropCommonPrefix startPath destinationPath
  in length (minPath \\ [start, destination])

solve = do
  orbits <- resultOrError <$> parseMap <$> readFile "../input/day06.txt"
  putStrLn "Part 1:"
  print $ countOrbits orbits
  putStrLn "Part 2:"
  print $ findMinimumTraversals orbits "YOU" "SAN"

test1 = do
  let orbits = resultOrError $ parseMap $ testInput1
  print $ countOrbits orbits

test2 = do
  let orbits = resultOrError $ parseMap $ testInput2
  print $ findMinimumTraversals orbits "YOU" "SAN"


testInput1 = "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L"

testInput2 = "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN"