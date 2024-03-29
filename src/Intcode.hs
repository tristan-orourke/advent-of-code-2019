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
paramModeOfDigit x = error ("Not a valid Parameter Mode code: " ++ show x)

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

data ParameterType = Value | Destination deriving (Show, Eq)

class Instruction a where
    len :: a -> Int
    values :: a -> [Int]
    paramModes :: a -> [ParameterMode]
    paramTypes :: a -> [ParameterType]
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

    values (Add a b c d) = [a,b,c,d]
    values (Mult a b c d) = [a,b,c,d]
    values (FromInput a b) = [a,b]
    values (ToOutput a b) = [a,b]
    values (JumpIfTrue a b c) = [a, b, c]
    values (JumpIfFalse a b c) = [a, b, c]
    values (LessThan a b c d) = [a, b, c, d]
    values (Equals a b c d) = [a, b, c, d]
    values (AdjustRelative a b) = [a, b]
    values End = [99]
    
    paramModes End = []
    paramModes i = 
        let (xs, _) = (splitAtOpcode . head . values) i in
            paramModesOfInts (len i - 1) xs
    
    paramTypes (Add _ _ _ _) = [Value, Value, Destination]
    paramTypes (Mult _ _ _ _) = [Value, Value, Destination]
    paramTypes (FromInput _ _) = [Destination]
    paramTypes (ToOutput _ _) = [Value]
    paramTypes (JumpIfTrue _ _ _) = [Value, Value]
    paramTypes (JumpIfFalse _ _ _) = [Value, Value]
    paramTypes (LessThan _ _ _ _) = [Value, Value, Destination]
    paramTypes (Equals _ _ _ _) = [Value, Value, Destination]
    paramTypes (AdjustRelative _ _) = [Value]
    paramTypes End = []

    readParamValues a pro = 
        let readValueParam pro mode x = case mode of
                Position -> readMem x pro
                Immediate -> x
                Relative -> readMem (x + relativeBase pro) pro 
            readDestParam pro mode x = case mode of
                Position -> x
                Immediate -> x
                Relative -> x + relativeBase pro
            in let readParamValue pro Value m x = readValueParam pro m x
                   readParamValue pro Destination m x = readDestParam pro m x
                    in zipWith3 (readParamValue pro) (paramTypes a) (paramModes a) (tail $ values a) 
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
                Add _ _ _ _ -> 
                    let [a, b, c] = readParamValues' in
                        setMem c (a + b) $ shiftPointer (len i) pro
                Mult _ _ _ _ -> 
                    let [a, b, c] = readParamValues' in
                        setMem c (a * b) $ shiftPointer (len i) pro
                FromInput _ a ->
                    if input pro == []
                        then pro { pause=True }
                        else let [a] = readParamValues'
                                 (x, pro') = popInput pro in
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
                LessThan x _ _ _ -> 
                    let [a, b, c] = readParamValues' in
                        let y = if a < b then 1 else 0 in
                            setMem c y $ shiftPointer (len i) pro 
                Equals _ _ _ _ ->
                    let [a, b, c] = readParamValues' in
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
-- >>> y = [109,1,203,4,99,0]
-- >>> q = initProgram [42] y
-- >>> q
-- >>> q' = runProgramStep q
-- >>> q'
-- >>> j = currentInstr q'
-- >>> j
-- >>> paramTypes j
-- >>> paramModes j
-- >>> values j
-- >>> readParamValues j q'
-- >>>
-- Program {memory = fromList [(0,1102),(1,34463338),(2,34463338),(3,63),(4,1007),(5,63),(6,34463338),(7,63),(8,1005),(9,63),(10,53),(11,1101),(12,3),(13,0),(14,1000),(15,109),(16,988),(17,209),(18,12),(19,9),(20,1000),(21,209),(22,6),(23,209),(24,3),(25,203),(26,0),(27,1008),(28,1000),(29,1),(30,63),(31,1005),(32,63),(33,65),(34,1008),(35,1000),(36,2),(37,63),(38,1005),(39,63),(40,904),(41,1008),(42,1000),(43,0),(44,63),(45,1005),(46,63),(47,58),(48,4),(49,25),(50,104),(51,0),(52,99),(53,4),(54,0),(55,104),(56,0),(57,99),(58,4),(59,17),(60,104),(61,0),(62,99),(63,0),(64,0),(65,1101),(66,27),(67,0),(68,1014),(69,1101),(70,286),(71,0),(72,1023),(73,1102),(74,1),(75,35),(76,1018),(77,1102),(78,20),(79,1),(80,1000),(81,1101),(82,26),(83,0),(84,1010),(85,1101),(86,0),(87,289),(88,1022),(89,1102),(90,1),(91,30),(92,1019),(93,1102),(94,734),(95,1),(96,1025),(97,1102),(98,1),(99,31),(100,1012),(101,1101),(102,25),(103,0),(104,1001),(105,1102),(106,1),(107,1),(108,1021),(109,1101),(110,0),(111,36),(112,1002),(113,1101),(114,0),(115,527),(116,1028),(117,1101),(118,895),(119,0),(120,1026),(121,1102),(122,1),(123,23),(124,1016),(125,1101),(126,21),(127,0),(128,1003),(129,1102),(130,22),(131,1),(132,1011),(133,1102),(134,1),(135,522),(136,1029),(137,1102),(138,1),(139,892),(140,1027),(141,1102),(142,1),(143,0),(144,1020),(145,1102),(146,1),(147,28),(148,1015),(149,1102),(150,38),(151,1),(152,1006),(153,1101),(154,0),(155,32),(156,1008),(157,1101),(158,743),(159,0),(160,1024),(161,1101),(162,0),(163,37),(164,1007),(165,1102),(166,1),(167,24),(168,1013),(169,1102),(170,1),(171,33),(172,1009),(173,1102),(174,39),(175,1),(176,1004),(177,1102),(178,1),(179,34),(180,1005),(181,1102),(182,1),(183,29),(184,1017),(185,109),(186,19),(187,21102),(188,40),(189,1),(190,-3),(191,1008),(192,1016),(193,40),(194,63),(195,1005),(196,63),(197,203),(198,4),(199,187),(200,1106),(201,0),(202,207),(203,1001),(204,64),(205,1),(206,64),(207,1002),(208,64),(209,2),(210,64),(211,109),(212,-7),(213,2101),(214,0),(215,-7),(216,63),(217,1008),(218,63),(219,32),(220,63),(221,1005),(222,63),(223,227),(224,1106),(225,0),(226,233),(227,4),(228,213),(229,1001),(230,64),(231,1),(232,64),(233,1002),(234,64),(235,2),(236,64),(237,109),(238,-3),(239,2108),(240,37),(241,-2),(242,63),(243,1005),(244,63),(245,255),(246,4),(247,239),(248,1001),(249,64),(250,1),(251,64),(252,1105),(253,1),(254,255),(255,1002),(256,64),(257,2),(258,64),(259,109),(260,11),(261,21108),(262,41),(263,40),(264,-6),(265,1005),(266,1014),(267,275),(268,1001),(269,64),(270,1),(271,64),(272,1106),(273,0),(274,277),(275,4),(276,261),(277,1002),(278,64),(279,2),(280,64),(281,109),(282,10),(283,2105),(284,1),(285,-7),(286,1105),(287,1),(288,295),(289,4),(290,283),(291,1001),(292,64),(293,1),(294,64),(295,1002),(296,64),(297,2),(298,64),(299,109),(300,-27),(301,1201),(302,-2),(303,0),(304,63),(305,1008),(306,63),(307,25),(308,63),(309,1005),(310,63),(311,321),(312,4),(313,301),(314,1001),(315,64),(316,1),(317,64),(318,1105),(319,1),(320,321),(321,1002),(322,64),(323,2),(324,64),(325,109),(326,15),(327,21107),(328,42),(329,41),(330,0),(331,1005),(332,1018),(333,341),(334,1001),(335,64),(336,1),(337,64),(338,1106),(339,0),(340,343),(341,4),(342,327),(343,1002),(344,64),(345,2),(346,64),(347,109),(348,-25),(349,2108),(350,20),(351,10),(352,63),(353,1005),(354,63),(355,359),(356,1105),(357,1),(358,365),(359,4),(360,349),(361,1001),(362,64),(363,1),(364,64),(365,1002),(366,64),(367,2),(368,64),(369,109),(370,12),(371,2107),(372,35),(373,0),(374,63),(375,1005),(376,63),(377,385),(378,1001),(379,64),(380,1),(381,64),(382,1106),(383,0),(384,387),(385,4),(386,371),(387,1002),(388,64),(389,2),(390,64),(391,109),(392,4),(393,21101),(394,43),(395,0),(396,6),(397,1008),(398,1015),(399,43),(400,63),(401,1005),(402,63),(403,409),(404,4),(405,393),(406,1106),(407,0),(408,413),(409,1001),(410,64),(411,1),(412,64),(413,1002),(414,64),(415,2),(416,64),(417,109),(418,9),(419,21101),(420,44),(421,0),(422,-8),(423,1008),(424,1010),(425,46),(426,63),(427,1005),(428,63),(429,437),(430,1001),(431,64),(432,1),(433,64),(434,1106),(435,0),(436,439),(437,4),(438,419),(439,1002),(440,64),(441,2),(442,64),(443,109),(444,5),(445,21108),(446,45),(447,45),(448,-4),(449,1005),(450,1019),(451,457),(452,4),(453,445),(454,1106),(455,0),(456,461),(457,1001),(458,64),(459,1),(460,64),(461,1002),(462,64),(463,2),(464,64),(465,109),(466,-22),(467,2102),(468,1),(469,7),(470,63),(471,1008),(472,63),(473,33),(474,63),(475,1005),(476,63),(477,481),(478,1106),(479,0),(480,487),(481,4),(482,467),(483,1001),(484,64),(485,1),(486,64),(487,1002),(488,64),(489,2),(490,64),(491,109),(492,14),(493,21102),(494,46),(495,1),(496,-1),(497,1008),(498,1014),(499,43),(500,63),(501,1005),(502,63),(503,507),(504,1106),(505,0),(506,513),(507,4),(508,493),(509,1001),(510,64),(511,1),(512,64),(513,1002),(514,64),(515,2),(516,64),(517,109),(518,12),(519,2106),(520,0),(521,1),(522,4),(523,519),(524,1106),(525,0),(526,531),(527,1001),(528,64),(529,1),(530,64),(531,1002),(532,64),(533,2),(534,64),(535,109),(536,-17),(537,1205),(538,10),(539,547),(540,1001),(541,64),(542,1),(543,64),(544,1106),(545,0),(546,549),(547,4),(548,537),(549,1002),(550,64),(551,2),(552,64),(553,109),(554,-8),(555,1202),(556,-2),(557,1),(558,63),(559,1008),(560,63),(561,17),(562,63),(563,1005),(564,63),(565,569),(566,1105),(567,1),(568,575),(569,4),(570,555),(571,1001),(572,64),(573,1),(574,64),(575,1002),(576,64),(577,2),(578,64),(579,109),(580,23),(581,1206),(582,-5),(583,593),(584,4),(585,581),(586,1001),(587,64),(588,1),(589,64),(590,1105),(591,1),(592,593),(593,1002),(594,64),(595,2),(596,64),(597,109),(598,-14),(599,1208),(600,-8),(601,24),(602,63),(603,1005),(604,63),(605,613),(606,1001),(607,64),(608,1),(609,64),(610,1105),(611,1),(612,615),(613,4),(614,599),(615,1002),(616,64),(617,2),(618,64),(619,109),(620,-2),(621,1207),(622,-1),(623,33),(624,63),(625,1005),(626,63),(627,633),(628,4),(629,621),(630,1105),(631,1),(632,637),(633,1001),(634,64),(635,1),(636,64),(637,1002),(638,64),(639,2),(640,64),(641,109),(642,2),(643,21107),(644,47),(645,48),(646,5),(647,1005),(648,1016),(649,659),(650,4),(651,643),(652,1001),(653,64),(654,1),(655,64),(656,1105),(657,1),(658,659),(659,1002),(660,64),(661,2),(662,64),(663,109),(664,-11),(665,1208),(666,8),(667,32),(668,63),(669,1005),(670,63),(671,681),(672,4),(673,665),(674,1001),(675,64),(676,1),(677,64),(678,1106),(679,0),(680,681),(681,1002),(682,64),(683,2),(684,64),(685,109),(686,2),(687,2101),(688,0),(689,0),(690,63),(691,1008),(692,63),(693,36),(694,63),(695,1005),(696,63),(697,703),(698,4),(699,687),(700,1106),(701,0),(702,707),(703,1001),(704,64),(705,1),(706,64),(707,1002),(708,64),(709,2),(710,64),(711,109),(712,12),(713,1206),(714,7),(715,719),(716,1106),(717,0),(718,725),(719,4),(720,713),(721,1001),(722,64),(723,1),(724,64),(725,1002),(726,64),(727,2),(728,64),(729,109),(730,2),(731,2105),(732,1),(733,8),(734,4),(735,731),(736,1001),(737,64),(738,1),(739,64),(740,1106),(741,0),(742,743),(743,1002),(744,64),(745,2),(746,64),(747,109),(748,-21),(749,2102),(750,1),(751,9),(752,63),(753,1008),(754,63),(755,39),(756,63),(757,1005),(758,63),(759,769),(760,4),(761,749),(762,1001),(763,64),(764,1),(765,64),(766,1105),(767,1),(768,769),(769,1002),(770,64),(771,2),(772,64),(773,109),(774,11),(775,1201),(776,-3),(777,0),(778,63),(779,1008),(780,63),(781,24),(782,63),(783,1005),(784,63),(785,793),(786,1001),(787,64),(788,1),(789,64),(790,1105),(791,1),(792,795),(793,4),(794,775),(795,1002),(796,64),(797,2),(798,64),(799,109),(800,20),(801,1205),(802,-5),(803,809),(804,4),(805,801),(806,1105),(807,1),(808,813),(809,1001),(810,64),(811,1),(812,64),(813,1002),(814,64),(815,2),(816,64),(817,109),(818,-23),(819,1207),(820,4),(821,36),(822,63),(823,1005),(824,63),(825,833),(826,1001),(827,64),(828,1),(829,64),(830,1105),(831,1),(832,835),(833,4),(834,819),(835,1002),(836,64),(837,2),(838,64),(839,109),(840,-3),(841,2107),(842,33),(843,5),(844,63),(845,1005),(846,63),(847,853),(848,4),(849,841),(850,1106),(851,0),(852,857),(853,1001),(854,64),(855,1),(856,64),(857,1002),(858,64),(859,2),(860,64),(861,109),(862,16),(863,1202),(864,-9),(865,1),(866,63),(867,1008),(868,63),(869,37),(870,63),(871,1005),(872,63),(873,879),(874,4),(875,863),(876,1105),(877,1),(878,883),(879,1001),(880,64),(881,1),(882,64),(883,1002),(884,64),(885,2),(886,64),(887,109),(888,12),(889,2106),(890,0),(891,-1),(892,1105),(893,1),(894,901),(895,4),(896,889),(897,1001),(898,64),(899,1),(900,64),(901,4),(902,64),(903,99),(904,21101),(905,0),(906,27),(907,1),(908,21101),(909,0),(910,915),(911,0),(912,1106),(913,0),(914,922),(915,21201),(916,1),(917,48476),(918,1),(919,204),(920,1),(921,99),(922,109),(923,3),(924,1207),(925,-2),(926,3),(927,63),(928,1005),(929,63),(930,964),(931,21201),(932,-2),(933,-1),(934,1),(935,21101),(936,0),(937,942),(938,0),(939,1105),(940,1),(941,922),(942,21202),(943,1),(944,1),(945,-1),(946,21201),(947,-2),(948,-3),(949,1),(950,21101),(951,0),(952,957),(953,0),(954,1105),(955,1),(956,922),(957,22201),(958,1),(959,-1),(960,-2),(961,1106),(962,0),(963,968),(964,21202),(965,-2),(966,1),(967,-2),(968,109),(969,-3),(970,2106),(971,0),(972,0)], pointer = 0, relativeBase = 0, input = [1], output = [], halt = False, pause = False}
-- *** Exception: Not a valid Parameter Mode code: 6
-- CallStack (from HasCallStack):
--   error, called at /home/squirx/dev/advent-of-code-2019/src/Intcode.hs:77:22 in main:Intcode
-- *** Exception: Not a valid Parameter Mode code: 6
-- CallStack (from HasCallStack):
--   error, called at /home/squirx/dev/advent-of-code-2019/src/Intcode.hs:77:22 in main:Intcode
-- Program {memory = fromList [(0,109),(1,1),(2,203),(3,4),(4,99),(5,0)], pointer = 0, relativeBase = 0, input = [42], output = [], halt = False, pause = False}
-- Program {memory = fromList [(0,109),(1,1),(2,203),(3,4),(4,99),(5,0)], pointer = 2, relativeBase = 1, input = [42], output = [], halt = False, pause = False}
-- FromInput 203 4
-- [Destination]
-- [Position]
-- [4]
-- [4]
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