module Day24 (solve, test) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.List (iterate')
import Data.Set (Set)
import qualified Data.Set as S

data Point = Point { pX :: Int, pY :: Int, level :: Int } deriving (Eq, Show)
instance Ord Point
  where (Point xa ya la) <= (Point xb yb lb) =
          if la /= lb then la <= lb
          else if yb == ya then xa < xb else ya < yb

isCenter (Point x y l) = x == 2 && y == 2

data Bug = Dead | Alive deriving (Eq, Show, Enum)
fromChar c = if c == '#' then Alive else Dead
toChar b = case b of Dead -> '.'; Alive -> '#'

liveOrDie c neighborCount = case (c, neighborCount) of (Alive, 1) -> Alive
                                                       (Alive, _) -> Dead
                                                       (Dead, 1) -> Alive
                                                       (Dead, 2) -> Alive
                                                       (Dead, _) -> Dead

readBugs :: String -> Map Point Bug
readBugs s = M.fromList $ concat $ zipWith makeLine (lines s) [0..]
  where makeLine line y = zipWith makeCoord (repeat y) (zip [0..] line)
        makeCoord y (x, c) = (Point x y 0, fromChar c)

solve = do
  gen0 <- readBugs <$> readFile "../input/day24.txt"
  putStrLn "Part 1:"
  print $ firstDuplicate $ map biodiversityRating $ iterate' nextGeneration gen0

  putStrLn "Part 2:"
  let gen200 = head $ drop 200 $ iterate' nextGeneration2 gen0
  print $ M.size gen200

{- Part 1 -}
above p@(Point x y l) = p { pY = y-1 }
below p@(Point x y l) = p { pY = y+1 }
left  p@(Point x y l) = p { pX = x-1 }
right p@(Point x y l) = p { pX = x+1 }

nextGeneration currentGeneration = M.mapWithKey nextGenerationAt currentGeneration
  where nextGenerationAt k c =
          let neighbors = [above,below,left,right] <*> [k]
              neighborCount = sum $ map fromEnum $ map (flip (M.findWithDefault Dead) currentGeneration) neighbors
          in liveOrDie c neighborCount

biodiversityRating bugs = sum $ zipWith (\b r -> case b of Dead -> 0; Alive -> r) (M.elems bugs) powersOfTwo
powersOfTwo = take 25 $ map (2^) [0..]

firstDuplicate :: [Int] -> Int
firstDuplicate s = firstDuplicate' s S.empty
  where firstDuplicate' (a:rest) seen =
          if S.member a seen then a
          else firstDuplicate' rest (S.insert a seen)

{- Part 2 -}
checkRecursion p@(Point x y l) oldP@(Point oldX oldY _)
  | y < 0 = [Point 2 1 (l-1)]
  | 4 < y = [Point 2 3 (l-1)]
  | x < 0 = [Point 1 2 (l-1)]
  | 4 < x = [Point 3 2 (l-1)]
  | isCenter p = case (oldX, oldY) of (_, 1) -> [Point x 0 (l+1) | x <- [0..4]]
                                      (_, 3) -> [Point x 4 (l+1) | x <- [0..4]]
                                      (1, _) -> [Point 0 y (l+1) | y <- [0..4]]
                                      (3, _) -> [Point 4 y (l+1) | y <- [0..4]]
  | otherwise = [p]

pAbove p@(Point x y l) = checkRecursion p { pY = y-1 } p
pBelow p@(Point x y l) = checkRecursion p { pY = y+1 } p
pLeft  p@(Point x y l) = checkRecursion p { pX = x-1 } p
pRight p@(Point x y l) = checkRecursion p { pX = x+1 } p

plutonianNeighbors p = concat $ [pAbove,pBelow,pLeft,pRight] <*> [p]

nextGeneration2 currentGeneration = next
   where next = M.fromList $ filter ((==Alive) . snd) $ map nextGenerationAt $ toConsider
         toConsider = concat $ map plutonianNeighbors $ M.keys currentGeneration
         nextGenerationAt p = let neighbors = plutonianNeighbors p
                                  neighborCount  = sum $ map fromEnum $ map (flip (M.findWithDefault Dead) currentGeneration) neighbors
                                  current = M.findWithDefault Dead p currentGeneration
                              in (p, liveOrDie current neighborCount)

{- Test stuff -}
test = do
  let gen0 = readBugs testInput
  let gen1 = nextGeneration gen0
  putStrLn $ printBugs 0 gen1
  putStrLn $ printBugs 0 $ nextGeneration gen1
  print $ biodiversityRating $ readBugs biodiversityExample

  let gen10 = head $ drop 10 $ iterate' nextGeneration2 gen0
  print $ M.size gen10

testInput = "....#\n#..#.\n#..##\n..#..\n#...."
biodiversityExample = ".....\n.....\n.....\n#....\n.#..."

printBugs :: Int -> Map Point Bug -> String
printBugs l grid =
  let keys = filter (\Point { level = pL } -> l == pL) $ M.keys grid
      tiles = unlines [[toChar (M.findWithDefault Dead (Point x y l) grid) | x <- [0..4]] | y <- [0..4]]
  in tiles