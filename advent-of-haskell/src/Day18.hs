{-# LANGUAGE RecordWildCards #-}
module Day18 (solve, test) where

import qualified Data.HashMap.Strict as M
import           Data.HashMap.Strict (HashMap)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Char (isLower, isUpper, toUpper)
import           Data.List (minimumBy, delete, (\\))
import           Data.Ord (comparing)
import           Data.Maybe (mapMaybe, isNothing, fromJust)
import           Data.Tuple (swap)
import           Debug.Trace (traceShow, traceShowId)

data Object = Entrance | Key Char | Door Char | Wall | Open deriving (Show, Eq)
fromChar :: Char -> Object
fromChar c
  | c == '@' = Entrance
  | c == '#' = Wall
  | c == '.' = Open
  | isLower c = Key c
  | isUpper c = Door c

isKey o = case o of Key _ -> True; _ -> False
isDoor o = case o of Door _ -> True; _ -> False
fromDoor (Door d) = d
fromKey (Key k) = k
matches (Door d) (Key k) = (toUpper k) == d
matches _ _ = False

above (x, y) = (x, y-1)
below (x, y) = (x, y+1)
left  (x, y) = (x-1, y)
right (x, y) = (x+1, y)

readCave :: String -> HashMap (Int, Int) Object
readCave s = (M.filter (/=Wall)) $ M.fromList $ concat $ zipWith makeLine (lines s) [0..]
  where makeLine line y = zipWith makeCoord (repeat y) (zip [0..] line)
        makeCoord y (x, c) = ((x, y), fromChar c)

visitAll :: ((Int, Int) -> Bool) -> (Int, Int) -> HashMap (Int, Int) Int
visitAll isLegalMove startAt = visitAll' [(startAt, 0)] M.empty
  where
    visitAll' [] visited = visited
    visitAll' ((pos, steps):rest) visited =
      let allMoves = [above,below,left,right] <*> [pos]
          newMoves = filter (not . (`M.member` visited)) allMoves
          legalMoves = filter isLegalMove newMoves
          steps' = steps + 1
          visited' = foldl (\m p -> M.insert p steps' m) visited legalMoves
      in visitAll' (rest ++ (zip legalMoves (repeat steps'))) visited'

isLegalMove cave keys next =
  let atNext = M.lookupDefault Wall next cave
  in atNext == Open || atNext == Entrance || isKey atNext || any (matches atNext) keys

ignoreDoors cave next = M.member next cave

data State = State { sDist :: Int, sPos :: (Int, Int), sKeys :: [Char] } deriving (Show, Eq)
instance Ord State where (<=) (State d1 _ k1) (State d2 _ k2) = if length k1 == length k2 then d1 <= d2 else length k2 <= length k1
getKeys State{..} = sKeys
getDist State{..} = sDist

shortestVisitOrder :: HashMap (Int, Int) Object -> (Int, Int) -> [((Int, Int), Object)] -> Maybe State
shortestVisitOrder cave entrance targets = shortestVisitOrder' (S.singleton (State 0 entrance [])) S.empty Nothing
  where
    shortestVisitOrder' toVisit considered shortestSoFar = 
      let (best@State{..}, v) = S.deleteFindMin toVisit
          remaining = filter (not . (`elem` sKeys) . fromKey . snd) targets
          allVisited = visitAll (isLegalMove cave (map Key sKeys)) sPos
          visitedKeys = mapMaybe (\r@(p, o) -> fmap ((,) r) (M.lookup p allVisited)) remaining
          newMoves = filter (not . (`S.member` considered) . getKeys) $
            map (\((p, o), dist) -> let newKeys = (fromKey o):sKeys in State (sDist + dist) p newKeys) visitedKeys
          tooLong = case shortestSoFar of Nothing -> False; Just x -> (getDist x) < sDist
      in
        if S.null toVisit then shortestSoFar
        else if tooLong then shortestVisitOrder' v considered shortestSoFar
        else if null remaining then
          let s' = case shortestSoFar of Nothing -> Just best; Just x -> Just (min x best)
          in traceShow s' $ shortestVisitOrder' v considered s'
        else shortestVisitOrder' (foldr S.insert v newMoves) (foldr S.insert considered (map getKeys newMoves)) shortestSoFar

getAllKeys cave =
  let entrance = head $ M.keys $ M.filter (==Entrance) cave
      needed = M.toList $ M.filter isKey cave
  in shortestVisitOrder cave entrance needed

solve = do
  cave <- readCave <$> readFile "../input/day18.txt"
  putStrLn "Part 1:"
  let order = getAllKeys cave
  print order

test = do
  putStrLn "smallExample1:"
  print $ getAllKeys (readCave smallExample1)
  putStrLn "largeExample1:"
  print $ getAllKeys (readCave largeExample1)
  putStrLn "largeExample2:"
  print $ getAllKeys (readCave largeExample2)
  putStrLn "largeExample3:"
  print $ getAllKeys (readCave largeExample3)
  putStrLn "largeExample4:"
  print $ getAllKeys (readCave largeExample4)

smallExample1 = "#########\n#b.A.@.a#\n#########"
largeExample1 = "########################\n#f.D.E.e.C.b.A.@.a.B.c.#\n######################.#\n#d.....................#\n########################"
largeExample2 = "########################\n#...............b.C.D.f#\n#.######################\n#.....@.a.B.c.d.A.e.F.g#\n########################"
largeExample3 = "#################\n#i.G..c...e..H.p#\n########.########\n#j.A..b...f..D.o#\n########@########\n#k.E..a...g..B.n#\n########.########\n#l.F..d...h..C.m#\n#################"
largeExample4 = "########################\n#@..............ac.GI.b#\n###d#e#f################\n###A#B#C################\n###g#h#i################\n########################"