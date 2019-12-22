{-# LANGUAGE RecordWildCards #-}
module Day18 (solve, test) where

import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Char (isLower, isUpper, toUpper, toLower)
import           Data.List (minimumBy)
import           Data.Ord (comparing)

data Object = Entrance | Key Char | Door Char | Wall | Open deriving (Show, Eq)
fromChar :: Char -> Object
fromChar c
  | c == '@' = Entrance
  | c == '#' = Wall
  | c == '.' = Open
  | isLower c = Key c
  | isUpper c = Door c

type Coord = (Int, Int)
type Keys = Set Char

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

readCave :: String -> Map Coord Object
readCave s = (M.filter (/=Wall)) $ M.fromList $ concat $ zipWith makeLine (lines s) [0..]
  where makeLine line y = zipWith makeCoord (repeat y) (zip [0..] line)
        makeCoord y (x, c) = ((x, y), fromChar c)

visitAll :: (Coord -> (Bool, Keys)) -> Coord -> Map Coord (Int, Keys)
visitAll isLegalMove startAt = visitAll' [(startAt, 0, S.empty)] M.empty
  where
    visitAll' [] visited = visited
    visitAll' ((pos, steps, keys):rest) visited =
      let allMoves = [above,below,left,right] <*> [pos]
          newMoves = filter (not . (`M.member` visited)) allMoves
          toLegalMoves m = map (\(p, (_, k)) -> (p, S.union keys k)) $ filter (fst . snd) $ zip m (map isLegalMove m)
          legalMoves = toLegalMoves newMoves
          steps' = steps + 1
          visited' = foldl (\m (p, k) -> M.insert p (steps', k) m) visited legalMoves
          getToVisit (p, k) s = (p, s, k)
      in visitAll' (rest ++ (zipWith getToVisit legalMoves (repeat steps'))) visited'

isLegalMove cave next =
  let atNext = M.findWithDefault Wall next cave
  in case atNext of Door d -> let key = toLower d in (True, S.singleton key)
                    Wall -> (False, S.empty)
                    Open -> (True, S.empty)
                    Entrance -> (True, S.empty)
                    Key _ -> (True, S.empty)

data State = State { sDist :: Int, atKey :: Char, sKeys :: Keys } deriving (Show, Eq)
instance Ord State where (<=) (State d1 _ _) (State d2 _ _) = d1 <= d2

getAllKeys cave =
  let entrance = head $ M.keys $ M.filter (==Entrance) cave
      keys = M.toList $ M.filter isKey cave
      allKeys = map (fromKey . snd) keys
      keyCount = length allKeys
      entranceToKeys = getKeyDistanceMap entrance
      keyToKeys = M.fromList $ zip allKeys $ map getKeyDistanceMap (map fst keys)
      getKeyDistanceMap from =
        let allDistances = visitAll (isLegalMove cave) from
            otherKeys = filter ((/=from) . fst) keys
        in  M.fromList $ zip (map (fromKey . snd) otherKeys) $ map (allDistances M.!) (map fst otherKeys)
      reachable haveKeys (_, required) = S.null $ required S.\\ haveKeys
      possibleStarts = S.fromList $ M.elems $ M.mapWithKey (\k (d, _) -> State d k (S.singleton k)) $ M.filter (reachable S.empty) $ entranceToKeys
      findBestPath toVisit bestPaths = 
        if S.null toVisit then
          minimumBy (comparing snd) $
          M.toList $ M.filterWithKey (\(_, keys) _ -> S.size keys == keyCount) bestPaths
        else let (candidate@State{..}, v) = S.deleteFindMin toVisit
                 posAndKeys = (atKey, sKeys)
                 isBetterPath = case M.lookup posAndKeys bestPaths of Nothing -> True
                                                                      Just d -> sDist < d
                 bestPaths' = M.insert posAndKeys sDist bestPaths
                 toVisit' = foldr S.insert v $
                   M.elems $
                   M.mapWithKey (\k (d, _) -> State (d + sDist) k (S.insert k sKeys)) $
                   M.filter (reachable sKeys) (keyToKeys M.! atKey)
             in if not isBetterPath then findBestPath v bestPaths
                else findBestPath toVisit' bestPaths'
  in findBestPath possibleStarts M.empty

solve = do
  cave <- readCave <$> readFile "../input/day18.txt"
  putStrLn "Part 1:"
  let order = getAllKeys cave
  print $ snd order

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