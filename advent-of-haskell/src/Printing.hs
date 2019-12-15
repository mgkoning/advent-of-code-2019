module Printing 
  (printGrid)
where

import Data.HashMap.Strict (HashMap, lookupDefault)
import qualified Data.HashMap.Strict as M

{- Creates a string representation of a grid, given a grid, a function
   to determine the character to show for different values, and a
   default value for coordinates that are missing in the given grid.
   The x coordinates are assumed to increased from left to right, the
   y coordinates are assumed to increase from top to bottom. -}
printGrid :: HashMap (Int, Int) t -> (t -> Char) -> t -> String
printGrid grid charForElem missingKeyValue =
  let keys = M.keys grid
      minX = minimum $ map fst keys
      maxX = maximum $ map fst keys
      minY = minimum $ map snd keys
      maxY = maximum $ map snd keys
      tiles = unlines [[charForElem (lookupDefault missingKeyValue (x,y) grid) | x <- [minX..maxX]] | y <- [minY..maxY]]
  in tiles