module Day20 (solve, test) where

import qualified Data.HashMap.Strict as M
import Data.HashMap.Strict (HashMap)
import Data.Char (isLetter)
import Data.Maybe (mapMaybe, catMaybes)
import Data.List (sortOn, groupBy)
import Data.Function (on)

type Coord = (Int, Int)
type Coord3 = (Int, Int, Int)
data Direction = In | Out deriving (Show, Eq)
data Object = Wall | Passage | Portal String Coord Direction deriving (Show, Eq)

above (x, y) = (x, y-1)
below (x, y) = (x, y+1)
left  (x, y) = (x-1, y)
right (x, y) = (x+1, y)

constructMaze :: String -> HashMap Coord Object
constructMaze s = M.union portals passages
  where makeLine line y = zipWith makeCoord (repeat y) (zip [0..] line)
        makeCoord y (x, c) = ((x, y), c)
        charMaze = M.fromList $ concat $ zipWith makeLine (lines s) [0..]
        wallCoords = M.keys $ M.filter (=='#') charMaze
        (minWallX, minWallY) = minimum wallCoords
        (maxWallX, maxWallY) = maximum wallCoords
        passages = M.map (const Passage) $ M.filter (=='.') charMaze
        portalParts = M.filter isLetter charMaze
        portals = M.fromList $ concat $ map toPortal $ M.toList portalParts
        toPortal a@(p@(x, y), c) =
          let neighbors = [right, below] <*> [p]
              companions = catMaybes $ zipWith (\n mc -> fmap ((,) n) mc) neighbors (map (`M.lookup` portalParts) neighbors)
              connectedPart a b =
                let getConnections a moves = zip (repeat a) (filter (`M.member` passages) (moves <*> [a]))
                in head $ (getConnections a [left, above]) ++ (getConnections b [right, below])
              inOrOut (x, y) = if x < minWallX || maxWallX < x || y < minWallY || maxWallY < y then Out else In
              makePortal (pa, ca) (pb, cb) =
                let (ppos, opos) = connectedPart pa pb in (ppos, Portal [ca, cb] opos (inOrOut ppos))
          in zipWith makePortal (repeat a) companions

quickestToTarget :: (Coord3 -> [Coord3]) -> Coord3 -> Coord3 -> Maybe Int
quickestToTarget getMoves startAt target = quickestToTarget' [(startAt, 0)] M.empty
  where
    quickestToTarget' [] _ = Nothing
    quickestToTarget' ((pos, steps):rest) visited =
      let newMoves = filter (not . (`M.member` visited)) $ getMoves pos
          steps' = steps + 1
          visited' = foldl (\m p -> M.insert p steps' m) visited newMoves
      in if pos == target then Just steps
         else quickestToTarget' (rest ++ (zip newMoves (repeat steps'))) visited'

getMazeMovesP1 :: HashMap Coord Object -> HashMap Coord Coord3 -> Coord3 -> [Coord3]
getMazeMovesP1 maze portalMoves pos@(x, y, l) =
  let allMoves = [above,below,left,right] <*> [(x, y)]
      toDest l p@(x, y) =
        let atPos = M.lookupDefault Wall p maze
        in case atPos of Wall -> Nothing
                         Passage -> Just (x, y, l)
                         Portal _ _ _ ->
                           do
                             (px, py, upOrDown) <- M.lookup p portalMoves
                             return (px, py, l)
  in mapMaybe (toDest l) allMoves


getMazeMovesP2 :: HashMap Coord Object -> HashMap Coord Coord3 -> Coord3 -> [Coord3]
getMazeMovesP2 maze portalMoves pos@(x, y, l) =
  let allMoves = [above,below,left,right] <*> [(x, y)]
      toDest l p@(x, y) =
        let atPos = M.lookupDefault Wall p maze
        in case atPos of Wall -> Nothing
                         Passage -> Just (x, y, l)
                         Portal _ _ d ->
                           if l == 0 && d == Out then Nothing
                           else do
                             (px, py, upOrDown) <- M.lookup p portalMoves
                             return (px, py, l + upOrDown)
  in mapMaybe (toDest l) allMoves

getPortalMoves maze =
  let portals = M.filter (\o -> case o of Portal _ _ _ -> True; _ -> False) maze
      levelFromDirection d = case d of In -> 1; Out -> -1
      toCoord3 (Portal _ (x, y) d) = (x, y, levelFromDirection d)
      makePairs [(posA, portalA), (posB, portalB)] = [(posA, toCoord3 portalB), (posB, toCoord3 portalA)]
      makePairs _ = []
      getLabel (Portal s _ _) = s
  in M.fromList $ concat $ map (makePairs) $ groupBy ((==) `on` (getLabel . snd)) $ sortOn (getLabel . snd) $ M.toList portals

findPortalOuts maze label =
  M.elems $ M.mapMaybe (\x -> case x of Portal s o _ -> if s == label then Just o else Nothing; _ -> Nothing) maze

part1 maze = do
  let (startX, startY) = head $ findPortalOuts maze "AA"
      (endX, endY) = head $ findPortalOuts maze "ZZ"
      portalMoves = getPortalMoves maze
      targetSteps = quickestToTarget (getMazeMovesP1 maze portalMoves) (startX, startY, 0) (endX, endY, 0)
  print targetSteps

part2 maze = do
  let (startX, startY) = head $ findPortalOuts maze "AA"
      (endX, endY) = head $ findPortalOuts maze "ZZ"
      portalMoves = getPortalMoves maze
      targetSteps = quickestToTarget (getMazeMovesP2 maze portalMoves) (startX, startY, 0) (endX, endY, 0)
  print targetSteps

solve = do
  maze <- constructMaze <$> readFile "../input/day20.txt"
  putStrLn "Part 1:"
  part1 maze
  putStrLn "Part 2:"
  part2 maze
  

test = do
  part1 $ constructMaze testInput1
  part1 $ constructMaze testInput2
  part2 $ constructMaze testInput1
  part2 $ constructMaze testInput3

testInput1 = "         A           \n         A           \n  #######.#########  \n  #######.........#  \n  #######.#######.#  \n  #######.#######.#  \n  #######.#######.#  \n  #####  B    ###.#  \nBC...##  C    ###.#  \n  ##.##       ###.#  \n  ##...DE  F  ###.#  \n  #####    G  ###.#  \n  #########.#####.#  \nDE..#######...###.#  \n  #.#########.###.#  \nFG..#########.....#  \n  ###########.#####  \n             Z       \n             Z       "
testInput2 = "                   A               \n                   A               \n  #################.#############  \n  #.#...#...................#.#.#  \n  #.#.#.###.###.###.#########.#.#  \n  #.#.#.......#...#.....#.#.#...#  \n  #.#########.###.#####.#.#.###.#  \n  #.............#.#.....#.......#  \n  ###.###########.###.#####.#.#.#  \n  #.....#        A   C    #.#.#.#  \n  #######        S   P    #####.#  \n  #.#...#                 #......VT\n  #.#.#.#                 #.#####  \n  #...#.#               YN....#.#  \n  #.###.#                 #####.#  \nDI....#.#                 #.....#  \n  #####.#                 #.###.#  \nZZ......#               QG....#..AS\n  ###.###                 #######  \nJO..#.#.#                 #.....#  \n  #.#.#.#                 ###.#.#  \n  #...#..DI             BU....#..LF\n  #####.#                 #.#####  \nYN......#               VT..#....QG\n  #.###.#                 #.###.#  \n  #.#...#                 #.....#  \n  ###.###    J L     J    #.#.###  \n  #.....#    O F     P    #.#...#  \n  #.###.#####.#.#####.#####.###.#  \n  #...#.#.#...#.....#.....#.#...#  \n  #.#####.###.###.#.#.#########.#  \n  #...#.#.....#...#.#.#.#.....#.#  \n  #.###.#####.###.###.#.#.#######  \n  #.#.........#...#.............#  \n  #########.###.###.#############  \n           B   J   C               \n           U   P   P               "
testInput3 = "             Z L X W       C                 \n             Z P Q B       K                 \n  ###########.#.#.#.#######.###############  \n  #...#.......#.#.......#.#.......#.#.#...#  \n  ###.#.#.#.#.#.#.#.###.#.#.#######.#.#.###  \n  #.#...#.#.#...#.#.#...#...#...#.#.......#  \n  #.###.#######.###.###.#.###.###.#.#######  \n  #...#.......#.#...#...#.............#...#  \n  #.#########.#######.#.#######.#######.###  \n  #...#.#    F       R I       Z    #.#.#.#  \n  #.###.#    D       E C       H    #.#.#.#  \n  #.#...#                           #...#.#  \n  #.###.#                           #.###.#  \n  #.#....OA                       WB..#.#..ZH\n  #.###.#                           #.#.#.#  \nCJ......#                           #.....#  \n  #######                           #######  \n  #.#....CK                         #......IC\n  #.###.#                           #.###.#  \n  #.....#                           #...#.#  \n  ###.###                           #.#.#.#  \nXF....#.#                         RF..#.#.#  \n  #####.#                           #######  \n  #......CJ                       NM..#...#  \n  ###.#.#                           #.###.#  \nRE....#.#                           #......RF\n  ###.###        X   X       L      #.#.#.#  \n  #.....#        F   Q       P      #.#.#.#  \n  ###.###########.###.#######.#########.###  \n  #.....#...#.....#.......#...#.....#.#...#  \n  #####.#.###.#######.#######.###.###.#.#.#  \n  #.......#.......#.#.#.#.#...#...#...#.#.#  \n  #####.###.#####.#.#.#.#.###.###.#.###.###  \n  #.......#.....#.#...#...............#...#  \n  #############.#.#.###.###################  \n               A O F   N                     \n               A A D   M                     "









