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

-- A Program consists of memory and a pointer
data Program = Program  { memory :: [Int]
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

readMem :: Int -> Program -> Int
readMem i pro = 
    if i >= length m 
        then 0 
        else m !! i
    where m = memory pro

readMemBlock :: Int -> Int -> Program -> [Int]
readMemBlock i n pro = rpad 0 n $ take n $ snd $ splitAt i m
        where m = memory pro

setMem :: Int -> Int -> Program -> Program
setMem i v pro = 
    let m = rpad 0 i (memory pro) in
        pro { memory = replaceAt m i v}

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
                Relative -> readMem (relativeBase pro + x) pro 
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

-- >>> x = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
-- >>> p = initProgram 1 x
-- >>> p
-- >>> i = currentInstr p
-- >>> i
-- >>> p2 = runNSteps 1 p
-- >>> p2
-- >>> i2 = currentInstr p2
-- >>> values i2
-- >>> paramModes i2
-- >>> readParamValues i2
-- Program {memory = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99], pointer = 0, relativeBase = 0, input = 1, output = [], halt = False}
-- AdjustRelative 109 1
-- Program {memory = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99], pointer = 2, relativeBase = 1, input = 1, output = [], halt = False}
-- [-1]
-- [Relative]
-- <BLANKLINE>
-- <interactive>:4282:2-19: error:
--     • No instance for (Show (Program -> [Int]))
--         arising from a use of ‘print’
--         (maybe you haven't applied a function to enough arguments?)
--     • In a stmt of an interactive GHCi command: print it
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
initProgram i m = Program {memory=m, pointer=0, relativeBase=0, input=i, output=[], halt=False, pause=False}

unpauseProgram :: Int -> Program -> Program
unpauseProgram i pro = runIntcodeProgram $ pushInput i pro { pause=False }

runIntcode :: [Int] -> [Int]
runIntcode m = memory $ runIntcodeProgram $ initProgram [1] m

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