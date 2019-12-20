module Day20 (solve, test) where

import qualified Data.HashMap.Strict as M
import Data.HashMap.Strict (HashMap)
import Data.Char (isLetter)
import Data.Maybe (mapMaybe, catMaybes)
import Data.List (sortOn, groupBy)
import Data.Function (on)

type Coord = (Int, Int)
data Object = Wall | Passage | Portal String Coord deriving (Show, Eq)

above (x, y) = (x, y-1)
below (x, y) = (x, y+1)
left  (x, y) = (x-1, y)
right (x, y) = (x+1, y)

constructMaze :: String -> HashMap Coord Object
constructMaze s = M.union portals passages
  where makeLine line y = zipWith makeCoord (repeat y) (zip [0..] line)
        makeCoord y (x, c) = ((x, y), c)
        charMaze = M.fromList $ concat $ zipWith makeLine (lines s) [0..]
        passages = M.map (const Passage) $ M.filter (=='.') charMaze
        portalParts = M.filter isLetter charMaze
        portals = M.fromList $ concat $ map toPortal $ M.toList portalParts
        toPortal a@(p@(x, y), c) =
          let neighbors = [right, below] <*> [p]
              companions = catMaybes $ zipWith (\n mc -> fmap ((,) n) mc) neighbors (map (`M.lookup` portalParts) neighbors)
              connectedPart a b =
                let getConnections a moves = zip (repeat a) (filter (`M.member` passages) (moves <*> [a]))
                in head $ (getConnections a [left, above]) ++ (getConnections b [right, below])
              makePortal (pa, ca) (pb, cb) =
                let (ppos, opos) = connectedPart pa pb in (ppos, Portal [ca, cb] opos)
          in zipWith makePortal (repeat a) companions

visitAll :: (Coord -> [Coord]) -> Coord -> HashMap Coord Int
visitAll getMoves startAt = visitAll' [(startAt, 0)] M.empty
  where
    visitAll' [] visited = visited
    visitAll' ((pos, steps):rest) visited =
      let newMoves = filter (not . (`M.member` visited)) $ getMoves pos
          steps' = steps + 1
          visited' = foldl (\m p -> M.insert p steps' m) visited newMoves
      in visitAll' (rest ++ (zip newMoves (repeat steps'))) visited'

getMazeMoves :: HashMap Coord Object -> HashMap Coord Coord -> Coord -> [Coord]
getMazeMoves maze portalMoves pos =
  let allMoves = [above,below,left,right] <*> [pos]
      toDest p =
        let atPos = M.lookupDefault Wall p maze
        in case atPos of Wall -> Nothing
                         Passage -> Just p
                         Portal _ _ -> M.lookup p portalMoves
  in mapMaybe toDest allMoves

getPortalMoves maze =
  let portals = M.filter (\o -> case o of Portal _ _ -> True; _ -> False) maze
      makePairs [(pa, Portal _ oa), (pb, Portal _ ob)] = [(pa, ob), (pb, oa)]
      makePairs _ = []
      getLabel (Portal s _) = s
  in M.fromList $ concat $ map (makePairs) $ groupBy ((==) `on` (getLabel . snd)) $ sortOn (getLabel . snd) $ M.toList portals

findPortalOuts maze label = M.elems $ M.mapMaybe (\x -> case x of Portal s o -> if s == label then Just o else Nothing; _ -> Nothing) maze

solve = do
  maze <- constructMaze <$> readFile "../input/day20.txt"
  putStrLn "Part 1:"
  let start = head $ findPortalOuts maze "AA"
      end = head $ findPortalOuts maze "ZZ"
      portalMoves = getPortalMoves maze
      allVisited = visitAll (getMazeMoves maze portalMoves) start
  print start
  print end
  print $ M.lookup end allVisited

test = do
  let maze = constructMaze testInput1
      start = head $ findPortalOuts maze "AA"
      end = head $ findPortalOuts maze "ZZ"
      portalMoves = getPortalMoves maze
      allVisited = visitAll (getMazeMoves maze portalMoves) start
  print start
  print end
  print $ M.lookup end allVisited
  let maze = constructMaze testInput2
      start = head $ findPortalOuts maze "AA"
      end = head $ findPortalOuts maze "ZZ"
      portalMoves = getPortalMoves maze
      allVisited = visitAll (getMazeMoves maze portalMoves) start
  print start
  print end
  print $ M.lookup end allVisited

testInput1 = "         A           \n         A           \n  #######.#########  \n  #######.........#  \n  #######.#######.#  \n  #######.#######.#  \n  #######.#######.#  \n  #####  B    ###.#  \nBC...##  C    ###.#  \n  ##.##       ###.#  \n  ##...DE  F  ###.#  \n  #####    G  ###.#  \n  #########.#####.#  \nDE..#######...###.#  \n  #.#########.###.#  \nFG..#########.....#  \n  ###########.#####  \n             Z       \n             Z       "
testInput2 = "                   A               \n                   A               \n  #################.#############  \n  #.#...#...................#.#.#  \n  #.#.#.###.###.###.#########.#.#  \n  #.#.#.......#...#.....#.#.#...#  \n  #.#########.###.#####.#.#.###.#  \n  #.............#.#.....#.......#  \n  ###.###########.###.#####.#.#.#  \n  #.....#        A   C    #.#.#.#  \n  #######        S   P    #####.#  \n  #.#...#                 #......VT\n  #.#.#.#                 #.#####  \n  #...#.#               YN....#.#  \n  #.###.#                 #####.#  \nDI....#.#                 #.....#  \n  #####.#                 #.###.#  \nZZ......#               QG....#..AS\n  ###.###                 #######  \nJO..#.#.#                 #.....#  \n  #.#.#.#                 ###.#.#  \n  #...#..DI             BU....#..LF\n  #####.#                 #.#####  \nYN......#               VT..#....QG\n  #.###.#                 #.###.#  \n  #.#...#                 #.....#  \n  ###.###    J L     J    #.#.###  \n  #.....#    O F     P    #.#...#  \n  #.###.#####.#.#####.#####.###.#  \n  #...#.#.#...#.....#.....#.#...#  \n  #.#####.###.###.#.#.#########.#  \n  #...#.#.....#...#.#.#.#.....#.#  \n  #.###.#####.###.###.#.#.#######  \n  #.#.........#...#.............#  \n  #########.###.###.#############  \n           B   J   C               \n           U   P   P               "









