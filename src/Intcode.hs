module Intcode
    (
        readPointerAtPointer,
        runIntcode
    ) where

import Util

-- A Program consists of memory and a pointer
data Program = Program [Int] Int

readPointerAtPointer :: [Int] -> Int -> Int
readPointerAtPointer m p = m !! (m !! p)

opFunc :: (Int -> Int -> Int) -> Program -> Program
opFunc f (Program m p) =
    let (a, b) = (readPointerAtPointer m (p+1), readPointerAtPointer m (p + 2)) in
    let newMem = replaceAt m (m !! (p+3)) (f a b) in
    Program newMem (p+4)

opAdd :: Program -> Program
opAdd = opFunc (+)

opMult :: Program -> Program
opMult = opFunc (*)

opEnd :: Program -> Program
opEnd (Program m p) = Program m (p+1)

runIntcodeRec :: Program -> Program
runIntcodeRec (Program m p) = 
    let program = Program m p in
    case m !! p of
        1 -> runIntcodeRec (opAdd program)
        2 -> runIntcodeRec (opMult program)
        99 -> opEnd program
        _ -> opEnd program

runIntcode :: [Int] -> [Int]
runIntcode input = 
    let (Program m p) = runIntcodeRec (Program input 0) in
    m

getIntcodeOutput :: [Int] -> Int
getIntcodeOutput = head . runIntcode

