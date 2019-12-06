module Intcode
    (
        runIntcode,
        runWithNounVerb,
    ) where

import Util
import Data.Maybe (listToMaybe)

-- A Program consists of memory and a pointer
data Program = Program  { memory :: [Int]
                        , pointer :: Int
                        , input :: Int
                        , output :: [Int]
                        , halt :: Bool
                        } deriving (Show, Eq)

readMemf :: (Int -> Int) -> Program -> Int
readMemf f pro = 
    let (m, p) = (memory pro, pointer pro) in
        m !! (f p)

readMem :: Program -> Int
readMem = readMemf id

data ParameterMode = Position | Immediate
readParamValue :: ParameterMode -> [Int] -> Int -> Int
readParamValue mode m p = 
    case mode of
        Position -> m !! (m !! p)
        Immediate -> m !! p

readPosValue :: [Int] -> Int -> Int
readPosValue = readParamValue Position

nextParamMode :: [Int] -> ParameterMode
nextParamMode [] = Position
nextParamMode (0:_) = Position
nextParamMode (1:_) = Immediate
nextParamMode _ = error "Not a valid Parameter Mode code"

opFunc :: (Int -> Int -> Int) -> Program -> Program
opFunc f program =
    let p = pointer program
        m = memory program in
        let (a, b) = (readPosValue m (p+1), readPosValue m (p + 2)) in
            let newMem = replaceAt m (m !! (p+3)) (f a b) in
                program { memory=newMem, pointer=(p+4) }

opAdd :: Program -> Program
opAdd = opFunc (+)

opMult :: Program -> Program
opMult = opFunc (*)

opEnd :: Program -> Program
opEnd p = p { pointer=(pointer p + 1), halt=True }

-- instrValues :: Program -> [Int]
-- instrValues pro = 
--     let ()

splitIntoParamsAndOpcode :: Int -> ([Int], Int)
splitIntoParamsAndOpcode d = 
    let ds = digits d in
        let (xs, ys) = splitAt (length ds - 2) ds in
            (xs, intOfDigits ys)

currentOpcode :: Program -> Int
currentOpcode pro = 
    let (_, x) = splitIntoParamsAndOpcode (readMem pro) in x

runProgramStep :: Program -> Program
runProgramStep pro = 
    if halt pro 
        then pro
        else
    let (paramModes, opcode) = splitIntoParamsAndOpcode (readMem pro) in
        case opcode of
            1 -> opAdd pro
            2 -> opMult pro
            99 -> opEnd pro
            _ -> opEnd pro

runIntcodeProgram :: Program -> Program
runIntcodeProgram pro = 
    if halt pro 
        then pro
        else runIntcodeProgram $ runProgramStep pro

initProgram :: Int -> [Int] -> Program
initProgram i m = Program {memory=m, pointer=0, input=i, output=[], halt=False}

runIntcode :: [Int] -> [Int]
runIntcode m = memory $ runIntcodeProgram $ initProgram 1 m

setNounVerb :: Int -> Int -> [Int] -> [Int]
setNounVerb n v = replaceIn n 1 . (replaceIn v 2)

runWithNounVerb :: Int -> Int -> [Int] -> [Int]
runWithNounVerb n v = runIntcode . (setNounVerb n v)


