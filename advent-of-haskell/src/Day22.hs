module Day22 (solve, test) where

import Data.List (foldl', elemIndex)
import Parsing
import Text.Parsec ((<|>), try)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (string)
import qualified Data.IntMap as M

data Technique = DealNewStack | Cut Int | DealWithIncrement Int deriving Show

parseShuffle = resultOrError . parseLines parseTechnique

parseTechnique :: Parser Technique
parseTechnique = Cut <$ string "cut " <*> parseInt
                 <|> try (DealNewStack <$ string "deal into new stack")
                 <|> DealWithIncrement <$ string "deal with increment " <*> parseInt

applyTechnique l s t =
  case t of DealNewStack -> reverse s
            Cut x -> applyCut l x s
            DealWithIncrement x -> applyDealWithIncrement l x s

applyCut l x s =
  let toDrop = if x < 0 then x + l else x
      (pref, suff) = splitAt toDrop s
  in suff ++ pref

applyDealWithIncrement l x s = M.elems $ M.fromList $ zipWith moveCard [0..] s
  where moveCard p c = ((p * x) `mod` l, c)

part1 len = foldl' (applyTechnique len) [0..len-1]

solve = do
  slamShuffle <- parseShuffle <$> readFile "../input/day22.txt"
  putStrLn "Part 1:"
  let shuffled = part1 10007 slamShuffle
  print $ elemIndex 2019 shuffled


test = do
  print $ part1 10 $ parseShuffle testShuffle1
  print $ part1 10 $ parseShuffle testShuffle2
  print $ part1 10 $ parseShuffle testShuffle3
  print $ part1 10 $ parseShuffle testShuffle4

testShuffle1 = "deal with increment 7\ndeal into new stack\ndeal into new stack"
testShuffle2 = "cut 6\ndeal with increment 7\ndeal into new stack"
testShuffle3 = "deal with increment 7\ndeal with increment 9\ncut -2"
testShuffle4 = "deal into new stack\ncut -2\ndeal with increment 7\ncut 8\ncut -4\ndeal with increment 7\ncut 3\ndeal with increment 9\ndeal with increment 3\ncut -1"