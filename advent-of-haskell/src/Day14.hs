{-# LANGUAGE RecordWildCards #-}

module Day14 (solve) where

import Parsing
import Text.Parsec (sepBy1, many1, letter, string)
import Data.List ((\\), nub)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M

data Reaction = Reaction { reactants :: [Reagent], yield :: Reagent } deriving (Show)
data Reagent = Reagent { quantity :: Int, material :: String } deriving (Show)

parseReactions s = resultOrError $ parseLines parseReaction s
  where parseReaction = mkReaction <$> sepBy1 parseReagent (string ", ") <* string " => " <*> parseReagent
        parseReagent = mkReagent <$> parseInt <* string " " <*> many1 letter
        mkReaction rs y = Reaction rs y
        mkReagent q m = Reagent q m

{- Simple topological sort to determine the order of reaction requirements -}
sortedReagents reactions =
  let edges = concatMap (\Reaction{..} -> zip (map material reactants) (repeat $ material yield)) reactions
  in topSort edges (noIncomingEdges edges) []
    where 
      noIncomingEdges e = (nub (map fst e)) \\ (nub (map snd e))
      topSort [] [] l = l
      topSort _ [] _ = error "graph not empty"
      topSort edges (n:s) l =
        let (edges', noInc) = removeEdges n edges
        in topSort edges' (nub (s ++ noInc)) (n:l)
      removeEdges n e =
        let toRemove = filter ((==n) . fst) e
            ms = nub $ map snd toRemove
            e' = e \\ toRemove
        in (e', filter (not . (`elem` (nub (map snd e')))) ms)

{- Given a list of reactions and the topologically sorted list of reagents,
   determine the materials needed to get the required fuel amount -}
requiredMaterials reactions sortedReagents requiredFuel =
  foldl addRequirement (M.singleton "FUEL" requiredFuel) sortedReagents
  where reactionLookup = M.fromList $ map (\r@Reaction{..} -> (material yield, r)) reactions
        addRequirement requirements material =
          if not $ M.member material reactionLookup then requirements
          else
            let requiredQuantity = requirements M.! material
                reaction = reactionLookup M.! material
                (q, r) = requiredQuantity `quotRem` (quantity $ yield reaction)
                factor = q + if 0 < r then 1 else 0
            in foldr (\Reagent{..} r -> M.insertWith (+) material (factor * quantity) r) requirements (reactants reaction)

{- Approximate the number of materials required starting from a previous approximation by the given step -}
getApprox reactions sorted from step =
  let approximations = map (\i -> (i, (M.! "ORE") $ requiredMaterials reactions sorted i)) [from,from+step..]
      bestApproximation = last $ takeWhile ((<=1000000000000) . snd) approximations
  in fst bestApproximation

solve = do
  reactions <- parseReactions <$> readFile "../input/day14.txt"
  putStrLn "Part 1:"
  let sorted = sortedReagents reactions
  let allRequired = requiredMaterials reactions sorted 1
  let oreRequired = allRequired M.! "ORE"
  print $ oreRequired
  putStrLn "Part 2:"
  let minimum = 1000000000000 `div` oreRequired
  let maxFuel = foldl (getApprox reactions sorted) minimum [1000,100,10,1]
  print maxFuel
