{-# LANGUAGE RecordWildCards #-}
module Day23 (solve) where

import Intcode
import Data.Maybe (listToMaybe, maybeToList, isNothing, fromJust)
import Data.List (partition, foldl')
import Data.List.Split (chunksOf)

data Packet = Packet { pRecipient :: Int, pX :: Int, pY :: Int } deriving (Show)
isNatPacket = intendedFor 255
intendedFor i Packet{..} = pRecipient == i
toInput Packet{..} = [pX, pY]
routeTo i p = p { pRecipient = i }

data NicState = NicState { iState :: State, outputBuffer :: [Int], nicAddress :: Int, nicIdle :: Bool }

data NetworkState = NetworkState { nics :: [NicState], packetsToRoute :: [Packet], natPacket :: Maybe Packet, allIdleFor :: Int }

emptyInput State{..} = null $ filter (/=(-1)) input

boot program = NetworkState (map bootNic [0..49]) [] Nothing 0
  where bootNic i = NicState (State [i] program 0 0) [] i False

step network@NetworkState{..} =
  let (releasedNatPackets, natPacket') =
        if 50 < allIdleFor then (maybeToList $ fmap (routeTo 0) natPacket, Nothing)
        else ([], natPacket)
      (nics', packets) = unzip $ map stepNic (updateInputs nics $ packetsToRoute ++ releasedNatPackets)
      (natPackets, packets') = partition isNatPacket $ concat packets
      newNatPacket = listToMaybe (natPackets ++ maybeToList natPacket')
      allIdle = all nicIdle nics'
      network' = NetworkState nics' packets' newNatPacket (if allIdle then allIdleFor + 1 else 0)
  in (network', releasedNatPackets):(step network')

updateInputs nics packetsToRoute = map updateInput nics
  where updateInput nic@NicState{..} =
          let packetsForMe = filter (intendedFor nicAddress) packetsToRoute
              newInputs = concat $ map toInput packetsForMe
              iState' =
                let s@State{..} = iState
                    input' = filter (/=(-1)) input ++ newInputs
                in s { input = if null input' then [-1] else input' } 
          in nic { iState = iState' }

stepNic nic@NicState{..} =
  let (iState', newOutput) = head $ doRunProgram iState
      idle = null newOutput && emptyInput iState'
      (packets, outputBuffer') = consumePackets $ outputBuffer ++ newOutput
  in (nic { iState = iState', outputBuffer = outputBuffer', nicIdle = idle }, packets)

consumePackets outputBuffer = foldl' readPacket ([], []) $ chunksOf 3 outputBuffer
  where readPacket (p, _) [r,x,y] = (p ++ [Packet r x y], [])
        readPacket (p, _) rem = (p, rem)

solve = do
  nicProgram <- readProgram <$> readFile "../input/day23.txt"
  putStrLn "Part 1:"
  let network = boot nicProgram
      runNetwork = step network
      firstBroadcast = head $ dropWhile isNothing $ map (natPacket . fst) runNetwork
  print $ pY $ fromJust firstBroadcast
  putStrLn "Part 2:"
  let releasedNatPackets = foldr (++) [] $ map snd $ runNetwork
  print $ firstRepeatBy pY releasedNatPackets

firstRepeatBy f (a:b:rest) = if f a == f b then f b else firstRepeatBy f (b:rest)