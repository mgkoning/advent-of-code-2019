{-# LANGUAGE RecordWildCards #-}
module Day18 (solve, test, combinations) where

import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Char (isLower, isUpper, toUpper, toLower)
import           Data.List (minimumBy, delete, (\\))
import           Data.Ord (comparing)
import           Data.Maybe (mapMaybe, isNothing, fromJust)
import           Data.Tuple (swap)
import           Debug.Trace (trace, traceShow, traceShowId)

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

ignoreDoors cave next = M.member next cave

data State = State { sDist :: Int, sPos :: (Int, Int), sKeys :: Keys } deriving (Show, Eq)
instance Ord State where (<=) (State d1 _ k1) (State d2 _ k2) = if S.size k1 == S.size k2 then d1 <= d2 else S.size k2 <= S.size k1
getKeys State{..} = sKeys
getDist State{..} = sDist

-- shortestVisitOrder :: HashMap (Int, Int) Object -> (Int, Int) -> [((Int, Int), Object)] -> Maybe State
-- shortestVisitOrder cave entrance targets = shortestVisitOrder' (S.singleton (State 0 entrance [])) S.empty Nothing
--   where
--     shortestVisitOrder' toVisit considered shortestSoFar = 
--       let (best@State{..}, v) = S.deleteFindMin toVisit
--           remaining = filter (not . (`elem` sKeys) . fromKey . snd) targets
--           allVisited = visitAll (isLegalMove cave (map Key sKeys)) sPos
--           visitedKeys = mapMaybe (\r@(p, o) -> fmap ((,) r) (M.lookup p allVisited)) remaining
--           newMoves = filter (not . (`S.member` considered) . getKeys) $
--             map (\((p, o), dist) -> let newKeys = (fromKey o):sKeys in State (sDist + dist) p newKeys) visitedKeys
--           tooLong = case shortestSoFar of Nothing -> False; Just x -> (getDist x) < sDist
--       in
--         if S.null toVisit then shortestSoFar
--         else if tooLong then shortestVisitOrder' v considered shortestSoFar
--         else if null remaining then
--           let s' = case shortestSoFar of Nothing -> Just best; Just x -> Just (min x best)
--           in traceShow s' $ shortestVisitOrder' v considered s'
--         else shortestVisitOrder' (foldr S.insert v newMoves) (foldr S.insert considered (map getKeys newMoves)) shortestSoFar

haveEnoughKeys keys (_, required) = S.null (required S.\\ keys)

--prune v dist = S.filter ((<dist) . getDist) v

combinations :: Int -> [a] -> [[a]]
combinations 0 list = [[]]
combinations _ [] = []
combinations n (x:xs) = map (x:) (combinations (n-1) xs) ++ combinations n xs

getAllKeys cave =
  let entrance = head $ M.keys $ M.filter (==Entrance) cave
      keys = M.toList $ M.filter isKey cave
      keyPosMap = M.fromList $ map (\(p, Key k) -> (k, p)) keys
      allKeys = map (fromKey . snd) keys
      keyCount = length allKeys
      getKeyDistanceMap from =
        let allDistances = visitAll (isLegalMove cave) from
            otherKeys = filter ((/=from) . fst) keys
        in  M.fromList $ zip (map (fromKey . snd) otherKeys) $ map (allDistances M.!) (map fst otherKeys)
      distancesToKeys =
        let pois = entrance:(map fst keys)
        in M.fromList $ zip pois $ map getKeyDistanceMap pois
      bestPath :: Int -> Map (Char, Keys) (Int, Keys) -> (Int, Keys)
      bestPath n previousOptima
        | keyCount < n = minimum $ M.elems previousOptima
        | otherwise =  let combs = combinations n allKeys -- [[Char]]
                           toVisit = concat $ map (\c -> [(k, c) | k <- c]) combs
                            
                           shortest :: (Char, [Char]) -> ((Char, Keys), (Int, Keys))
                           shortest (to, includes) = minimumBy (comparing $ fst . snd) $ map (getDistance to) (delete to includes)
                             where getDistance to from = 
                                     let (d, needed) = (distancesToKeys M.! (keyPosMap M.! from)) M.! to
                                         (prefixDist, prefixKeys) = previousOptima M.! (from, S.fromList (delete to includes))
                                     in ((to, S.fromList includes), toDistance (S.insert from prefixKeys) (d + prefixDist, needed))
                           newOptima = M.fromList $ map shortest toVisit
                       in bestPath (n+1) newOptima
      toDistance :: Keys -> (Int, Keys) -> (Int, Keys)
      toDistance haveKeys (dist, keys) = if S.null (keys S.\\ haveKeys) then (dist, haveKeys) else (10000000, S.empty)
  in bestPath 2 $ M.fromList $ map (\k -> ((k, S.singleton k), (toDistance S.empty) ((distancesToKeys M.! entrance) M.! k))) allKeys
    --shortestVisitOrder (S.singleton $ State 0 entrance S.empty) Nothing
    --zip keys $ map (`M.lookup` (distances M.! entrance)) kPos
      -- shortestVisitOrder toVisit bestSoFar =
      --   let (best@State{..}, v) = S.deleteFindMin toVisit
      --       remainingKeys = S.toList $ allKeys S.\\ sKeys
      --       options = filter ((haveEnoughKeys sKeys) . snd) $
      --         zip remainingKeys $ map ((distancesToKeys M.! sPos) M.!) $ remainingKeys
      --       toVisit' = foldr S.insert v $ map (\(k, (d, _)) -> State (sDist + d) (keyPosMap M.! k) (S.insert k sKeys)) options
      --   in if S.null toVisit then bestSoFar
      --      else if maybe False ((<sDist) . getDist) bestSoFar then shortestVisitOrder v bestSoFar
      --      else 
      --        if S.size sKeys == S.size allKeys
      --          then let newBest = maybe best (`min` best) bestSoFar
      --               in traceShow newBest $ shortestVisitOrder (prune v $ getDist newBest) $ Just newBest
      --        else shortestVisitOrder toVisit' bestSoFar
  --     needed = M.toList $ M.filter isKey cave
  -- in shortestVisitOrder cave entrance needed

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