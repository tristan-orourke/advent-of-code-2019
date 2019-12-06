module Intcode
    (
        readPointerAtPointer,
        runIntcode,
        runWithNounVerb,
    ) where

import Util

-- A Program consists of memory and a pointer
data Program = Program  { memory :: [Int]
                        , pointer :: Int
                        , input :: Int
                        , output :: [Int]
                        } deriving (Show, Eq)

readPointerAtPointer :: [Int] -> Int -> Int
readPointerAtPointer m p = m !! (m !! p)

opFunc :: (Int -> Int -> Int) -> Program -> Program
opFunc f program =
    let p = pointer program
        m = memory program in
        let (a, b) = (readPointerAtPointer m (p+1), readPointerAtPointer m (p + 2)) in
            let newMem = replaceAt m (m !! (p+3)) (f a b) in
                program { memory=newMem, pointer=(p+4) }

opAdd :: Program -> Program
opAdd = opFunc (+)

opMult :: Program -> Program
opMult = opFunc (*)

opEnd :: Program -> Program
opEnd p = p { pointer=(pointer p + 1) }

currentOpcode :: Program -> Int
currentOpcode p = 
    let (m, p') = (memory p, pointer p) in
        m !! p'

runIntcodeProgram :: Program -> Program
runIntcodeProgram p = 
    case currentOpcode p of
        1 -> runIntcodeProgram (opAdd p)
        2 -> runIntcodeProgram (opMult p)
        99 -> opEnd p
        _ -> opEnd p

initProgram :: Int -> [Int] -> Program
initProgram i m = Program {memory=m, pointer=0, input=i, output=[]}

runIntcode :: [Int] -> [Int]
runIntcode m = memory $ runIntcodeProgram $ initProgram 1 m

setNounVerb :: Int -> Int -> [Int] -> [Int]
setNounVerb n v = replaceIn n 1 . (replaceIn v 2)

runWithNounVerb :: Int -> Int -> [Int] -> [Int]
runWithNounVerb n v = runIntcode . (setNounVerb n v)


