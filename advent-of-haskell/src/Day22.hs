module Day22 (solve, test, egcd) where

import Data.List (foldl', elemIndex, iterate', genericSplitAt)
import Parsing
import Text.Parsec ((<|>), try)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (string)
import qualified Data.Map as M
import Data.Maybe (fromJust)

data Technique = DealNewStack | Cut Integer | DealWithIncrement Integer deriving Show

parseShuffle = resultOrError . parseLines parseTechnique

parseTechnique :: Parser Technique
parseTechnique = Cut <$ string "cut " <*> (toInteger <$> parseInt)
                 <|> try (DealNewStack <$ string "deal into new stack")
                 <|> DealWithIncrement <$ string "deal with increment " <*>  (toInteger <$> parseInt)

applyTechnique :: Integer -> [Integer] -> Technique -> [Integer]
applyTechnique l s t =
  case t of DealNewStack -> reverse s
            Cut x -> applyCut l x s
            DealWithIncrement x -> applyDealWithIncrement l x s

applyTechnique' :: Integer -> Integer -> Technique -> Integer
applyTechnique' l c t =
  case t of DealNewStack -> ((-c)-1) `mod` l
            Cut x -> (c - x) `mod` l
            DealWithIncrement x -> (c * x) `mod` l

applyCut :: Integer -> Integer -> [Integer] -> [Integer]
applyCut l x s =
  let toDrop = if x < 0 then x + l else x
      (pref, suff) = genericSplitAt toDrop s
  in suff ++ pref

applyDealWithIncrement :: Integer -> Integer -> [Integer] -> [Integer]
applyDealWithIncrement l x s = M.elems $ M.fromList $ zipWith moveCard [0..] s
  where moveCard p c = ((p * x) `mod` l, c)

part1 len deck = foldl' (applyTechnique len) deck

part1' s len card = foldl' (applyTechnique' len) card s

getFactors :: Technique -> (Integer, Integer)
getFactors t = case t of DealNewStack -> (-1, -1)
                         Cut x -> (1, -x)
                         DealWithIncrement x -> (x, 0)

-- combines factors of f(x) = ax + b with g(x) = cx + b as f . g
combineFactors n (a, b) (c, d) = (a*c `mod` n, (a*d + b) `mod` n)

combineSelf m n f = if n == 1 then f
                    else if n `mod` 2 == 0 then combineSelf m (n `div` 2) $ combineFactors m f f
                    else combineFactors m f $ (combineSelf m (n `div` 2) $ combineFactors m f f)
                         

egcd a b = egcd' 1 0 0 1 a b
  where egcd' x0 x1 y0 y1 a 0 = (x0, y0, x1, y1, a)
        egcd' x0 x1 y0 y1 a b = let (quot, remA) = a `quotRem` b
                                    xn = x0 - quot * x1
                                    yn = y0 - quot * y1
                                in egcd' x1 xn y1 yn b remA

applyTechniques2 l s i = foldr (applyTechnique2 l) i s

applyTechnique2 l t c = case t of Cut x -> (c + x) `mod` l
                                  DealNewStack -> l - 1 - c
                                  DealWithIncrement x ->
                                    if c == 0 then 0
                                    else let (f, _, _, _, gcd) = egcd x l
                                          in if gcd /= 1 then error "cannot invert"
                                             else f * c `mod` l

part2 cardAt len rep shuffle = take (rep+1) $ iterate' (applyTechniques2 len shuffle) cardAt

solve = do
  slamShuffle <- parseShuffle <$> readFile "../input/day22.txt"
  putStrLn "Part 1:"
  let shuffled = part1 10007 (deck 10007) slamShuffle
  print $ fromJust $ elemIndex 2019 shuffled
  putStrLn "Part 1 (alternative):"
  let fwdShuffle = iterate' (part1' slamShuffle 10007) 2019
  print $ head $ drop 1 $ fwdShuffle
  putStrLn "Part 2:"

  {- First, transform all operations to a pair of coefficients (a, b) denoting f(x) = ax + b.
     Combine all of these in an efficient manner (using exponentiation by squaring).
     Finally, solve cx + d = 2020 for x using the calculated coefficients (c, d).
     Note that since we use modular arithmetic, we have to find the inverse modular 
     multiplicative. For that, we use the extended euclidian algorithm. -}
  let (a, b) = foldr1 (combineFactors 119315717514047) $ map getFactors $ reverse slamShuffle
  let (c, d) = combineSelf 119315717514047 101741582076661 (a, b)
  let (invMod, _, _, _, _) = egcd c 119315717514047
  print $ (((2020 - d) * invMod) `mod` 119315717514047)

  {- It was worth a shot, but obviously not possible. -}
  --let theBigShuffle = part2 2020 119315717514047 101741582076661 slamShuffle
  --print $ last $ part2 2020 119315717514047 101741582076661 slamShuffle

deck len = [0..len-1]

test = do
  print $ part1 10 (deck 10) $ parseShuffle testShuffle1
  print $ part1 10 (deck 10) $ parseShuffle testShuffle2
  print $ part1 10 (deck 10) $ parseShuffle testShuffle3
  print $ part1 10 (deck 10) $ parseShuffle testShuffle4

testShuffle1 = "deal with increment 7\ndeal into new stack\ndeal into new stack"
testShuffle2 = "cut 6\ndeal with increment 7\ndeal into new stack"
testShuffle3 = "deal with increment 7\ndeal with increment 9\ncut -2"
testShuffle4 = "deal into new stack\ncut -2\ndeal with increment 7\ncut 8\ncut -4\ndeal with increment 7\ncut 3\ndeal with increment 9\ndeal with increment 3\ncut -1"