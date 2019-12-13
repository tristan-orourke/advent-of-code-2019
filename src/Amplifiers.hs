module Amplifiers (
    maxFiveAmplifiersOutput,
    maxBasicLoop,
) where

import Intcode
import Data.List (permutations, replicate)

runAmplifiers :: [Int] -> Int -> [Int] -> Int
runAmplifiers _ i [] = i
runAmplifiers m i (p:ps) =
    let output = outputWithQueuedInputs [p,i] m in
        runAmplifiers m output ps

runLoop :: Int -> [Program] -> Int
runLoop i (p:ps) =
    if halt p 
        then i
        else let p' = unpauseProgram i p in
            runLoop (head $ output p') (ps ++ [p'])
            
runAmplifierLoop :: [Int] -> Int -> [Int] -> Int
runAmplifierLoop m i ps =
    let pros = map ((flip initProgram m) . replicate 1) ps in
        runLoop i pros 

-- | Max possible amplifier output, when considering all phase permutations
maxAmplifierOutput :: [Int] -> Int -> [Int] -> Int
maxAmplifierOutput _ i [] = i
maxAmplifierOutput m i ps = maximum $ map (runAmplifiers m i) (permutations ps)

maxAmplifierLoopOutput :: [Int] -> Int -> [Int] -> Int
maxAmplifierLoopOutput m i ps = maximum $ map (runAmplifierLoop m i) (permutations ps)

maxFiveAmplifiersOutput :: [Int] -> Int
maxFiveAmplifiersOutput m = maxAmplifierOutput m 0 [0,1,2,3,4]

maxBasicLoop :: [Int] -> Int
maxBasicLoop m = maxAmplifierLoopOutput m 0 [5..9]
