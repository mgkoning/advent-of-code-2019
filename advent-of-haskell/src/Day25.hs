module Day25 (solve) where

import Intcode
import Data.List (isPrefixOf)
import Data.Char (chr, ord)
import Control.Monad (when)

solve = do
  adventure <- readProgram <$> readFile "../input/day25.txt"
  putStrLn "Part 1:"
  let initialState = State [] adventure 0 0
  loop initialState
  putStrLn "Bye!"

loop :: State -> IO ()
loop state = do
  let (say, state', continue) = readToPrompt state
  putStrLn say
  when continue $
    do input <- getLine
       when (input /= ":q") $
         do loop state' { input = map ord (input ++ "\n") }

readToPrompt s = go s []
  where
    go s output =
      let res = take 1 $ doRunProgram s
      in case res of
          [] -> (reverse output, s, False)
          [(s', [])] -> go s' output
          [(s', [o])] ->
            let c = chr o
            in if c == '?' && (reverse "Command") `isPrefixOf` output
                  then (reverse (c:output), s', True)
                  else go s' (c:output)