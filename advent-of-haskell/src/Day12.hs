{-# LANGUAGE RecordWildCards #-}
module Day12 (solve, test) where

import Parsing
import Text.Parsec (string)
import Text.Parsec.String (Parser)
import Data.List (iterate')
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M

type Position = (Int, Int, Int)
type Velocity = (Int, Int, Int)
data Planet = Planet { position :: Position, velocity :: Velocity } deriving (Show)
zeroPlanet = Planet (0, 0, 0) (0, 0, 0)

add (x, y, z) (a, b, c) = (x+a, y+b, z+c)
diff (x, y, z) (a, b, c) = (diff x a, diff y b, diff z c)
  where diff l r
          | l < r  = 1
          | l == r = 0
          | l > r  = -1
invert (x, y, z) = (-x, -y, -z)

parsePlanets s = parseLines parsePlanet s
  where parsePlanet = mkPlanet <$
                        string "<x=" <*> parseInt <*
                        string ", y=" <*> parseInt <*
                        string ", z=" <*> parseInt <* string ">"
        mkPlanet x y z = zeroPlanet { position = (x, y, z) }

simulate planets = iterate' update planets
  where update planets = map move $ applyPairwiseGravity planets
        move planet@Planet{..} = planet { position = add position velocity }
        applyPairwiseGravity [] = []
        applyPairwiseGravity (p:ps) = let (p', ps') = foldl grav (p, []) ps in [p'] ++ (applyPairwiseGravity ps')
        grav (b, as) a =
          let posA = position a
              posB = position b 
              dv = diff (posB) (posA)
          in (b { velocity = add (velocity b) dv }, as ++ [a { velocity = add (velocity a) (invert dv) }])

energy planets = sum $ map energy' planets
  where energy' Planet{ position = (x, y, z), velocity = (dx, dy, dz) } = (abs x + abs y + abs z) * (abs dx + abs dy + abs dz)

findCycle steps axis = findCycle' (zip steps [0..]) M.empty
  where findCycle' ((s, i):ss) seenBefore =
          let key = map (\p -> (axis $ position p, axis $ velocity p)) s
          in case M.lookup key seenBefore of
               Nothing -> findCycle' ss (M.insert key () seenBefore)
               Just _ -> i

solve = do
  planets <- resultOrError <$> parsePlanets <$> readFile "../input/day12.txt"
  putStrLn "Part 1:"
  let steps = simulate planets
  print $ energy $ head $ drop 1000 $ steps
  putStrLn "Part 2:"
  {- Since the axes do not influence each other at all, one can find cycles per axis. -}
  {- To determine convergence of all three axes, calculate the least common multiple. -}
  let xCycle = findCycle steps (\(x, _, _) -> x)
  let yCycle = findCycle steps (\(_, y, _) -> y)
  let zCycle = findCycle steps (\(_, _, z) -> z)
  print $ lcm xCycle $ lcm yCycle zCycle

test = do
  let planets = resultOrError $ parsePlanets $ testPlanets2
  let steps = simulate planets
  putStrLn $ unlines $ map show $ take 5 $ steps

testPlanets1 = "<x=-1, y=0, z=2>\n<x=2, y=-10, z=-7>\n<x=4, y=-8, z=8>\n<x=3, y=5, z=-1>"
testPlanets2 = "<x=-8, y=-10, z=0>\n<x=5, y=5, z=10>\n<x=2, y=-7, z=3>\n<x=9, y=-8, z=-3>"