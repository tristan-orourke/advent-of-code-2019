module Intcode
    (
        Program (..),
        initProgram,
        unpauseProgram,
        runIntcode,
        runWithNounVerb,
        outputOfRunProgram,
        outputAllRunProgram,
        outputWithQueuedInputs,
    ) where

import Util
import Data.Maybe (listToMaybe)
import qualified Data.Map.Strict as Map

-- A Program consists of memory and a pointer
data Program = Program  { memory :: Map.Map Int Int
                        , pointer :: Int
                        , relativeBase :: Int
                        , input :: [Int]
                        , output :: [Int]
                        , halt :: Bool
                        , pause :: Bool
                        } deriving (Show, Eq)

-- class IProgram p where
--     readMem :: Program -> Int -> Int
--     readMemBlock :: Program 

memoryList :: Program -> [Int]
memoryList pro = map snd $ Map.toAscList (memory pro)

readMem :: Int -> Program -> Int
readMem i pro = 
    Map.findWithDefault 0 i (memory pro)

readMemBlock :: Int -> Int -> Program -> [Int]
readMemBlock i n pro = map (flip readMem pro) [i..(i+n-1)]

setMem :: Int -> Int -> Program -> Program
setMem i v pro = 
    let m = memory pro in
        pro { memory = Map.insert i v m }

shiftPointer :: Int -> Program -> Program
shiftPointer n pro =
    let p = pointer pro in
        pro { pointer = (p + n) }

shiftRelativeBase :: Int -> Program -> Program
shiftRelativeBase n pro =
    let r = relativeBase pro in
        pro { relativeBase = (r + n) }

addOutput :: Int -> Program -> Program
addOutput v pro = 
    let o = output pro in
        pro { output = (v:o) }

pushInput :: Int -> Program -> Program
pushInput i pro =
    let i' = input pro in
        pro { input = (i' ++ [i]) }

popInput :: Program -> (Int, Program)
popInput pro =
    let (i:is) = input pro in
        (i, pro { input=is })

data ParameterMode = Position | Immediate | Relative deriving (Show, Eq)

paramModeOfDigit :: Int -> ParameterMode
paramModeOfDigit 0 = Position
paramModeOfDigit 1 = Immediate
paramModeOfDigit 2 = Relative
paramModeOfDigit _ = error "Not a valid Parameter Mode code"

paramModesOfInts :: Int -> [Int] -> [ParameterMode]
paramModesOfInts n xs = map paramModeOfDigit $ reverse $ lpad 0 n xs

splitIntoParamsAndOpcode :: Int -> ([ParameterMode], Int)
splitIntoParamsAndOpcode d = 
    let ds = digits d in
        let (xs, ys) = splitAt (length ds - 2) ds in
            (map paramModeOfDigit $ reverse $ lpad 0 3 xs, intOfDigits ys)

splitAtOpcode :: Int -> ([Int], Int)
splitAtOpcode d = 
    let ds = digits d in
        let (xs, ys) = splitAt (length ds - 2) ds in
            (xs, intOfDigits ys)


class Instruction a where
    len :: a -> Int
    values :: a -> [Int]
    paramModes :: a -> [ParameterMode]
    readParamValues :: a -> Program -> [Int]
    opcode :: a -> Int
    runInstr :: a -> Program -> Program

-- Opcode 3 takes a single integer as input and saves it to the position 
-- given by its only parameter. 
-- For example, the instruction 3,50 would take an input value 
-- and store it at address 50.

data Instr = Add Int Int Int Int
            | Mult Int Int Int Int
            | FromInput Int Int
            | ToOutput Int Int
            | JumpIfTrue Int Int Int
            | JumpIfFalse Int Int Int
            | LessThan Int Int Int Int
            | Equals Int Int Int Int
            | AdjustRelative Int Int
            | End

    deriving (Show, Eq)

instance Instruction Instr where
    len (Add _ _ _ _) = 4
    len (Mult _ _ _ _) = 4
    len (FromInput _ _) = 2
    len (ToOutput _ _) = 2
    len (JumpIfTrue _ _ _) = 3
    len (JumpIfFalse _ _ _) = 3
    len (LessThan _ _ _ _) = 4
    len (Equals _ _ _ _) = 4
    len (AdjustRelative _ _) = 2
    len (End) = 1

    values (Add _ a b c) = [a,b,c]
    values (Mult _ a b c) = [a,b,c]
    values (FromInput _ a) = [a]
    values (ToOutput _ a) = [a]
    values (JumpIfTrue _ a b) = [a, b]
    values (JumpIfFalse _ a b) = [a, b]
    values (LessThan _ a b c) = [a, b, c]
    values (Equals _ a b c) = [a, b, c]
    values (AdjustRelative _ a) = [a]
    values (End) = []
    
    paramModes i = case i of
        (Add x _ _ _) -> getModes i x
        (Mult x _ _ _) -> getModes i x
        (FromInput _ _) -> [Immediate]
        (ToOutput x _) -> getModes i x
        (JumpIfTrue x _ _) -> getModes i x
        (JumpIfFalse x _ _) -> getModes i x
        (LessThan x _ _ _) -> getModes i x
        (Equals x _ _ _) -> getModes i x
        (AdjustRelative x _) -> getModes i x
        (End) -> []
        where getModes i x = let (xs, _) = splitAtOpcode x in
                                paramModesOfInts (len i - 1) xs
    readParamValues a pro = 
        let readParamValue pro mode x = case mode of
                Position -> readMem x pro
                Immediate -> x
                Relative -> readMem (x + relativeBase pro) pro 
            in zipWith (readParamValue pro) (paramModes a) (values a) 
    opcode (Add _ _ _ _) = 1
    opcode (Mult _ _ _ _) = 2
    opcode (FromInput _ _) = 3
    opcode (ToOutput _ _) = 4
    opcode (JumpIfTrue _ _ _) = 5
    opcode (JumpIfFalse _ _ _) = 6
    opcode (LessThan _ _ _ _) = 7
    opcode (Equals _ _ _ _) = 8
    opcode (AdjustRelative _ _) = 9
    opcode End = 99

    runInstr i pro = 
        let readParamValues' = readParamValues i pro in
            case i of
                Add _ _ _ c -> 
                    let [a, b, _] = readParamValues' in
                        setMem c (a + b) $ shiftPointer (len i) pro
                Mult _ _ _ c -> 
                    let [a, b, _] = readParamValues' in
                        setMem c (a * b) $ shiftPointer (len i) pro
                FromInput _ a ->
                    if input pro == []
                        then pro { pause=True }
                        else let (x, pro') = popInput pro in
                            setMem a x $ shiftPointer (len i) pro'
                ToOutput _ _ ->
                    let [a] = readParamValues' in
                        addOutput a $ shiftPointer (len i) pro
                JumpIfTrue x _ _ -> 
                    let [a, b] = readParamValues' in
                        if a /= 0 then pro { pointer=b } else shiftPointer (len i) pro
                JumpIfFalse x _ _ ->
                    let [a, b] = readParamValues' in
                        if a == 0 then pro { pointer=b } else shiftPointer (len i) pro
                LessThan x _ _ c -> 
                    let [a, b, _] = readParamValues' in
                        let y = if a < b then 1 else 0 in
                            setMem c y $ shiftPointer (len i) pro 
                Equals _ _ _ c ->
                    let [a, b, _] = readParamValues' in
                    let y = if a == b then 1 else 0 in
                            setMem c y $ shiftPointer (len i) pro 
                AdjustRelative _ _ -> 
                    let [a] = readParamValues' in
                        shiftRelativeBase a $ shiftPointer (len i) pro
                End -> shiftPointer (len i) (pro {halt = True})

currentInstr :: Program -> Instr
currentInstr pro = 
    let p = pointer pro
        [a, b, c, d] = readMemBlock p 4 pro in
        let (_, x) = splitIntoParamsAndOpcode a 
            in case x of
                1 -> Add a b c d
                2 -> Mult a b c d
                3 -> FromInput a b
                4 -> ToOutput a b
                5 -> JumpIfTrue a b c
                6 -> JumpIfFalse a b c
                7 -> LessThan a b c d
                8 -> Equals a b c d
                9 -> AdjustRelative a b
                99 -> End
                _ -> End

-- >>> x = [1102,34463338,34463338,63,1007,63,34463338,63,1005,63,53,1101,3,0,1000,109,988,209,12,9,1000,209,6,209,3,203,0,1008,1000,1,63,1005,63,65,1008,1000,2,63,1005,63,904,1008,1000,0,63,1005,63,58,4,25,104,0,99,4,0,104,0,99,4,17,104,0,99,0,0,1101,27,0,1014,1101,286,0,1023,1102,1,35,1018,1102,20,1,1000,1101,26,0,1010,1101,0,289,1022,1102,1,30,1019,1102,734,1,1025,1102,1,31,1012,1101,25,0,1001,1102,1,1,1021,1101,0,36,1002,1101,0,527,1028,1101,895,0,1026,1102,1,23,1016,1101,21,0,1003,1102,22,1,1011,1102,1,522,1029,1102,1,892,1027,1102,1,0,1020,1102,1,28,1015,1102,38,1,1006,1101,0,32,1008,1101,743,0,1024,1101,0,37,1007,1102,1,24,1013,1102,1,33,1009,1102,39,1,1004,1102,1,34,1005,1102,1,29,1017,109,19,21102,40,1,-3,1008,1016,40,63,1005,63,203,4,187,1106,0,207,1001,64,1,64,1002,64,2,64,109,-7,2101,0,-7,63,1008,63,32,63,1005,63,227,1106,0,233,4,213,1001,64,1,64,1002,64,2,64,109,-3,2108,37,-2,63,1005,63,255,4,239,1001,64,1,64,1105,1,255,1002,64,2,64,109,11,21108,41,40,-6,1005,1014,275,1001,64,1,64,1106,0,277,4,261,1002,64,2,64,109,10,2105,1,-7,1105,1,295,4,283,1001,64,1,64,1002,64,2,64,109,-27,1201,-2,0,63,1008,63,25,63,1005,63,321,4,301,1001,64,1,64,1105,1,321,1002,64,2,64,109,15,21107,42,41,0,1005,1018,341,1001,64,1,64,1106,0,343,4,327,1002,64,2,64,109,-25,2108,20,10,63,1005,63,359,1105,1,365,4,349,1001,64,1,64,1002,64,2,64,109,12,2107,35,0,63,1005,63,385,1001,64,1,64,1106,0,387,4,371,1002,64,2,64,109,4,21101,43,0,6,1008,1015,43,63,1005,63,409,4,393,1106,0,413,1001,64,1,64,1002,64,2,64,109,9,21101,44,0,-8,1008,1010,46,63,1005,63,437,1001,64,1,64,1106,0,439,4,419,1002,64,2,64,109,5,21108,45,45,-4,1005,1019,457,4,445,1106,0,461,1001,64,1,64,1002,64,2,64,109,-22,2102,1,7,63,1008,63,33,63,1005,63,481,1106,0,487,4,467,1001,64,1,64,1002,64,2,64,109,14,21102,46,1,-1,1008,1014,43,63,1005,63,507,1106,0,513,4,493,1001,64,1,64,1002,64,2,64,109,12,2106,0,1,4,519,1106,0,531,1001,64,1,64,1002,64,2,64,109,-17,1205,10,547,1001,64,1,64,1106,0,549,4,537,1002,64,2,64,109,-8,1202,-2,1,63,1008,63,17,63,1005,63,569,1105,1,575,4,555,1001,64,1,64,1002,64,2,64,109,23,1206,-5,593,4,581,1001,64,1,64,1105,1,593,1002,64,2,64,109,-14,1208,-8,24,63,1005,63,613,1001,64,1,64,1105,1,615,4,599,1002,64,2,64,109,-2,1207,-1,33,63,1005,63,633,4,621,1105,1,637,1001,64,1,64,1002,64,2,64,109,2,21107,47,48,5,1005,1016,659,4,643,1001,64,1,64,1105,1,659,1002,64,2,64,109,-11,1208,8,32,63,1005,63,681,4,665,1001,64,1,64,1106,0,681,1002,64,2,64,109,2,2101,0,0,63,1008,63,36,63,1005,63,703,4,687,1106,0,707,1001,64,1,64,1002,64,2,64,109,12,1206,7,719,1106,0,725,4,713,1001,64,1,64,1002,64,2,64,109,2,2105,1,8,4,731,1001,64,1,64,1106,0,743,1002,64,2,64,109,-21,2102,1,9,63,1008,63,39,63,1005,63,769,4,749,1001,64,1,64,1105,1,769,1002,64,2,64,109,11,1201,-3,0,63,1008,63,24,63,1005,63,793,1001,64,1,64,1105,1,795,4,775,1002,64,2,64,109,20,1205,-5,809,4,801,1105,1,813,1001,64,1,64,1002,64,2,64,109,-23,1207,4,36,63,1005,63,833,1001,64,1,64,1105,1,835,4,819,1002,64,2,64,109,-3,2107,33,5,63,1005,63,853,4,841,1106,0,857,1001,64,1,64,1002,64,2,64,109,16,1202,-9,1,63,1008,63,37,63,1005,63,879,4,863,1105,1,883,1001,64,1,64,1002,64,2,64,109,12,2106,0,-1,1105,1,901,4,889,1001,64,1,64,4,64,99,21101,0,27,1,21101,0,915,0,1106,0,922,21201,1,48476,1,204,1,99,109,3,1207,-2,3,63,1005,63,964,21201,-2,-1,1,21101,0,942,0,1105,1,922,21202,1,1,-1,21201,-2,-3,1,21101,0,957,0,1105,1,922,22201,1,-1,-2,1106,0,968,21202,-2,1,-2,109,-3,2106,0,0]
-- >>> p = initProgram [1] x
-- >>> p
-- >>> i = currentInstr p
-- >>> output (runNSteps 5000 p)
-- >>> outputAllRunProgram 1 x
-- Program {memory = [1102,34463338,34463338,63,1007,63,34463338,63,1005,63,53,1101,3,0,1000,109,988,209,12,9,1000,209,6,209,3,203,0,1008,1000,1,63,1005,63,65,1008,1000,2,63,1005,63,904,1008,1000,0,63,1005,63,58,4,25,104,0,99,4,0,104,0,99,4,17,104,0,99,0,0,1101,27,0,1014,1101,286,0,1023,1102,1,35,1018,1102,20,1,1000,1101,26,0,1010,1101,0,289,1022,1102,1,30,1019,1102,734,1,1025,1102,1,31,1012,1101,25,0,1001,1102,1,1,1021,1101,0,36,1002,1101,0,527,1028,1101,895,0,1026,1102,1,23,1016,1101,21,0,1003,1102,22,1,1011,1102,1,522,1029,1102,1,892,1027,1102,1,0,1020,1102,1,28,1015,1102,38,1,1006,1101,0,32,1008,1101,743,0,1024,1101,0,37,1007,1102,1,24,1013,1102,1,33,1009,1102,39,1,1004,1102,1,34,1005,1102,1,29,1017,109,19,21102,40,1,-3,1008,1016,40,63,1005,63,203,4,187,1106,0,207,1001,64,1,64,1002,64,2,64,109,-7,2101,0,-7,63,1008,63,32,63,1005,63,227,1106,0,233,4,213,1001,64,1,64,1002,64,2,64,109,-3,2108,37,-2,63,1005,63,255,4,239,1001,64,1,64,1105,1,255,1002,64,2,64,109,11,21108,41,40,-6,1005,1014,275,1001,64,1,64,1106,0,277,4,261,1002,64,2,64,109,10,2105,1,-7,1105,1,295,4,283,1001,64,1,64,1002,64,2,64,109,-27,1201,-2,0,63,1008,63,25,63,1005,63,321,4,301,1001,64,1,64,1105,1,321,1002,64,2,64,109,15,21107,42,41,0,1005,1018,341,1001,64,1,64,1106,0,343,4,327,1002,64,2,64,109,-25,2108,20,10,63,1005,63,359,1105,1,365,4,349,1001,64,1,64,1002,64,2,64,109,12,2107,35,0,63,1005,63,385,1001,64,1,64,1106,0,387,4,371,1002,64,2,64,109,4,21101,43,0,6,1008,1015,43,63,1005,63,409,4,393,1106,0,413,1001,64,1,64,1002,64,2,64,109,9,21101,44,0,-8,1008,1010,46,63,1005,63,437,1001,64,1,64,1106,0,439,4,419,1002,64,2,64,109,5,21108,45,45,-4,1005,1019,457,4,445,1106,0,461,1001,64,1,64,1002,64,2,64,109,-22,2102,1,7,63,1008,63,33,63,1005,63,481,1106,0,487,4,467,1001,64,1,64,1002,64,2,64,109,14,21102,46,1,-1,1008,1014,43,63,1005,63,507,1106,0,513,4,493,1001,64,1,64,1002,64,2,64,109,12,2106,0,1,4,519,1106,0,531,1001,64,1,64,1002,64,2,64,109,-17,1205,10,547,1001,64,1,64,1106,0,549,4,537,1002,64,2,64,109,-8,1202,-2,1,63,1008,63,17,63,1005,63,569,1105,1,575,4,555,1001,64,1,64,1002,64,2,64,109,23,1206,-5,593,4,581,1001,64,1,64,1105,1,593,1002,64,2,64,109,-14,1208,-8,24,63,1005,63,613,1001,64,1,64,1105,1,615,4,599,1002,64,2,64,109,-2,1207,-1,33,63,1005,63,633,4,621,1105,1,637,1001,64,1,64,1002,64,2,64,109,2,21107,47,48,5,1005,1016,659,4,643,1001,64,1,64,1105,1,659,1002,64,2,64,109,-11,1208,8,32,63,1005,63,681,4,665,1001,64,1,64,1106,0,681,1002,64,2,64,109,2,2101,0,0,63,1008,63,36,63,1005,63,703,4,687,1106,0,707,1001,64,1,64,1002,64,2,64,109,12,1206,7,719,1106,0,725,4,713,1001,64,1,64,1002,64,2,64,109,2,2105,1,8,4,731,1001,64,1,64,1106,0,743,1002,64,2,64,109,-21,2102,1,9,63,1008,63,39,63,1005,63,769,4,749,1001,64,1,64,1105,1,769,1002,64,2,64,109,11,1201,-3,0,63,1008,63,24,63,1005,63,793,1001,64,1,64,1105,1,795,4,775,1002,64,2,64,109,20,1205,-5,809,4,801,1105,1,813,1001,64,1,64,1002,64,2,64,109,-23,1207,4,36,63,1005,63,833,1001,64,1,64,1105,1,835,4,819,1002,64,2,64,109,-3,2107,33,5,63,1005,63,853,4,841,1106,0,857,1001,64,1,64,1002,64,2,64,109,16,1202,-9,1,63,1008,63,37,63,1005,63,879,4,863,1105,1,883,1001,64,1,64,1002,64,2,64,109,12,2106,0,-1,1105,1,901,4,889,1001,64,1,64,4,64,99,21101,0,27,1,21101,0,915,0,1106,0,922,21201,1,48476,1,204,1,99,109,3,1207,-2,3,63,1005,63,964,21201,-2,-1,1,21101,0,942,0,1105,1,922,21202,1,1,-1,21201,-2,-3,1,21101,0,957,0,1105,1,922,22201,1,-1,-2,1106,0,968,21202,-2,1,-2,109,-3,2106,0,0], pointer = 0, relativeBase = 0, input = [1], output = [], halt = False, pause = False}
-- [0,203]
-- [203,0]
--

runProgramStep :: Program -> Program
runProgramStep pro = 
    if halt pro || pause pro
        then pro
        else runInstr (currentInstr pro) pro

runNSteps :: Int -> Program -> Program
runNSteps n pro = 
    if n <= 0 
        then pro
        else runNSteps (n-1) (runProgramStep pro)

runIntcodeProgram :: Program -> Program
runIntcodeProgram pro = 
    if halt pro || pause pro
        then pro
        else runIntcodeProgram $ runProgramStep pro

initProgram :: [Int] -> [Int] -> Program
initProgram i m = Program {memory=mMap, pointer=0, relativeBase=0, input=i, output=[], halt=False, pause=False}
        where mMap = Map.fromAscList $ zip [0..(length m - 1)] m

unpauseProgram :: Int -> Program -> Program
unpauseProgram i pro = runIntcodeProgram $ pushInput i pro { pause=False }

runIntcode :: [Int] -> [Int]
runIntcode m = memoryList $ runIntcodeProgram $ initProgram [1] m

outputOfRunProgram :: Int -> [Int] -> Int
outputOfRunProgram input = head . output . runIntcodeProgram . (initProgram [input])

outputWithQueuedInputs :: [Int] -> [Int] -> Int
outputWithQueuedInputs input mem = head $ output $ runIntcodeProgram $ initProgram input mem

outputAllRunProgram :: Int -> [Int] -> [Int]
outputAllRunProgram input = reverse . output . runIntcodeProgram . (initProgram [input])

setNounVerb :: Int -> Int -> [Int] -> [Int]
setNounVerb n v = replaceIn n 1 . (replaceIn v 2)

runWithNounVerb :: Int -> Int -> [Int] -> [Int]
runWithNounVerb n v = runIntcode . (setNounVerb n v)