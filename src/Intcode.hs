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

setMem :: Int -> Int -> Program -> Program
setMem i v pro = 
    let m = memory pro in
        pro { memory = replaceAt m i v}

movePointer :: Int -> Program -> Program
movePointer n pro =
    let p = pointer pro in
        pro { pointer = (p + n) }

data ParameterMode = Position | Immediate deriving (Show, Eq)
readParamValue :: ParameterMode -> [Int] -> Int -> Int
readParamValue mode m p = 
    case mode of
        Position -> m !! (m !! p)
        Immediate -> m !! p

paramModeOfDigit :: Int -> ParameterMode
paramModeOfDigit 0 = Position
paramModeOfDigit 1 = Immediate
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
    readParamValues :: a -> [Int] -> [Int]
    opcode :: a -> Int

-- Opcode 3 takes a single integer as input and saves it to the position 
-- given by its only parameter. 
-- For example, the instruction 3,50 would take an input value 
-- and store it at address 50.

data Instr = Add Int Int Int Int
            | Mult Int Int Int Int
            | FromInput Int Int
            | End

    deriving (Show, Eq)

instance Instruction Instr where
    len (Add _ _ _ _) = 4
    len (Mult _ _ _ _) = 4
    len (FromInput _ _) = 2
    len (End) = 1
    values (Add _ a b c) = [a,b,c]
    values (Mult _ a b c) = [a,b,c]
    values (FromInput _ a) = [a]
    values (End) = []
    paramModes i = case i of
        (Add x _ _ _) -> let (xs, _) = splitAtOpcode x in
                            paramModesOfInts (len i) xs
        (Mult x _ _ _) -> let (xs, _) = splitAtOpcode x in
                            paramModesOfInts (len i) xs
        (FromInput _ _) -> [Immediate]
        (End) -> []
    readParamValues a m = 
        let readParamValue' m mode x = case mode of
                Position -> m !! x
                Immediate -> x
            in zipWith (readParamValue' m) (paramModes a) (values a) 
    opcode (Add _ _ _ _) = 1
    opcode (Mult _ _ _ _) = 2
    opcode (FromInput _ _) = 3
    opcode End = 99

runInstr :: Instr -> Program -> Program
runInstr i pro = 
    let m = memory pro in
        case i of
            Add _ _ _ c -> 
                let [a, b, _] = readParamValues i m in
                    setMem c (a + b) $ movePointer (len i) pro
            Mult _ _ _ c -> 
                let [a, b, _] = readParamValues i m in
                    setMem c (a * b) $ movePointer (len i) pro
            FromInput _ a ->
                setMem a (input pro) $ movePointer (len i) pro
            End -> movePointer (len i) (pro {halt = True})

currentInstr :: Program -> Instr
currentInstr pro = 
    let (m, p) = (memory pro, pointer pro) in
        let (_, x) = splitIntoParamsAndOpcode (m !! p) 
            [a, b, c, d] = take 4 $ snd $ splitAt p m
            in case x of
                1 -> Add a b c d
                2 -> Mult a b c d
                3 -> FromInput a b
                99 -> End
                _ -> End

-- >>> x = initProgram 1 [1,0,0,0,99]
-- >>> x
-- >>> i = currentInstr x
-- >>> splitIntoParamsAndOpcode 1
-- >>> runProgramStep x
-- >>> readParamValues i (memory x)
-- Program {memory = [1,0,0,0,99], pointer = 0, input = 1, output = [], halt = False}
-- ([Position,Position,Position],1)
-- Program {memory = [1,2,0,0,99], pointer = 4, input = 1, output = [], halt = False}
-- [1,1,1]
--

runProgramStep :: Program -> Program
runProgramStep pro = 
    if halt pro 
        then pro
        else runInstr (currentInstr pro) pro

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


