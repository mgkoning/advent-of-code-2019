{-# LANGUAGE RecordWildCards #-}
module Day23 (solve) where

import Intcode
import Data.List (foldl', iterate')
import Data.List.Split (chunksOf)

data Packet = Packet { pRecipient :: Int, pX :: Int, pY :: Int } deriving (Show)
data NicState = NicState { iState :: State, outputBuffer :: [Int], nicAddress :: Int }
data NetworkState = NetworkState { nics :: [NicState], packetsToRoute :: [Packet] }

boot program = NetworkState (map bootNic [0..49]) []
  where bootNic i = NicState (State [i] program 0 0) [] i

step network@NetworkState{..} =
  let (nics', packets) = unzip $ map stepNic (updateInputs nics packetsToRoute)
      network' = NetworkState nics' (concat packets)
  in network':(step network')

updateInputs nics packetsToRoute = map updateInput nics
  where updateInput nic@NicState{..} =
          let packetsForMe = filter ((==nicAddress) . pRecipient) packetsToRoute
              newInputs = concat $ map (\Packet{..} -> [pX, pY]) packetsForMe
              iState' =
                let s@State{..} = iState
                    input' = filter (/=(-1)) input ++ newInputs
                in s { input = if null input' then [-1] else input' } 
          in nic { iState = iState' }

stepNic nic@NicState{..} =
  let (iState', newOutput) = head $ doRunProgram iState
      (packets, outputBuffer') = consumePackets $ outputBuffer ++ newOutput
  in (nic { iState = iState', outputBuffer = outputBuffer' }, packets)

consumePackets outputBuffer = foldl' readPacket ([], []) $ chunksOf 3 outputBuffer
  where readPacket (p, _) [r,x,y] = (p ++ [Packet r x y], [])
        readPacket (p, _) rem = (p, rem)

getBroadcasts NetworkState{..} = filter ((==255) . pRecipient) packetsToRoute

solve = do
  nicProgram <- readProgram <$> readFile "../input/day23.txt"
  let network = boot nicProgram
  putStrLn "Part 1:"
  let runNetwork = step network
      firstBroadcast = head $ dropWhile null $ map getBroadcasts runNetwork
  print $ head firstBroadcast