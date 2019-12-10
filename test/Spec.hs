import Test.Hspec
import Lib
import Intcode
import Wires
import Util
import Passwords
import Tree
import Orbits

main :: IO ()
main = hspec $ do 
    describe "calcFuel" $ do
        it "12 returns 2" $
            calcFuel 12 `shouldBe` 2
        it "14 returns 2" $
            calcFuel 14 `shouldBe` 2
        it "1969 returns 654" $
            calcFuel 1969 `shouldBe` 654
        it "100756 returns 33583" $
            calcFuel 100756 `shouldBe` 33583
    describe "calcFuelRec" $ do
        it "14 returns 2" $
            calcFuelRec 14 `shouldBe` 2
        it "1969 returns 966" $
            calcFuelRec 1969 `shouldBe` 966
        it "100756 returns 50346" $
            calcFuelRec 100756 `shouldBe` 50346
    
    describe "runIntcode" $ do
        it "1,0,0,0,99 becomes 2,0,0,0,99" $
            runIntcode [1,0,0,0,99] `shouldBe` [2,0,0,0,99]
        it "2,3,0,3,99 becomes 2,3,0,6,99" $
            runIntcode [2,3,0,3,99] `shouldBe` [2,3,0,6,99]
        it "2,4,4,5,99,0 becomes 2,4,4,5,99,9801" $
            runIntcode [2,4,4,5,99,0] `shouldBe` [2,4,4,5,99,9801]
        it "1,1,1,4,99,5,6,0,99 becomes 30,1,1,4,2,5,6,0,99" $
            runIntcode [1,1,1,4,99,5,6,0,99] `shouldBe` [30,1,1,4,2,5,6,0,99]
        it "1002,4,3,4,33 becomes 1002,4,3,4,99" $
            runIntcode [1002,4,3,4,33] `shouldBe` [1002,4,3,4,99]
        it "1101,100,-1,4,0 becomes 1101,100,-1,4,99" $
            runIntcode [1101,100,(-1),4,0] `shouldBe` [1101,100,(-1),4,99]
        it "3,3,99,0 becomes 3,3,99,1" $
            runIntcode [3,3,99,0] `shouldBe` [3,3,99,1]
    describe "Intcode.outputOfRunProgram" $ do
        it "4,3,99,6 outputs 6" $
            outputOfRunProgram 1 [4,3,99,6] `shouldBe` 6
        it "3,9,8,9,10,9,4,9,99,-1,8 outputs True if input equals 8" $
            outputOfRunProgram 8 [3,9,8,9,10,9,4,9,99,-1,8] `shouldBe` 1
        it "3,9,8,9,10,9,4,9,99,-1,8 outputs False if input doesn't equal 8" $
            outputOfRunProgram 7 [3,9,8,9,10,9,4,9,99,-1,8] `shouldBe` 0
        it "3,9,7,9,10,9,4,9,99,-1,8 outputs True if input less than 8" $
            outputOfRunProgram 3 [3,9,7,9,10,9,4,9,99,-1,8] `shouldBe` 1
        it "3,9,7,9,10,9,4,9,99,-1,8 outputs False if input doesn't equal 8" $
            outputOfRunProgram 8 [3,9,7,9,10,9,4,9,99,-1,8] `shouldBe` 0
        it "3,3,1108,-1,8,3,4,3,99 outputs True if input equals 8" $
            outputOfRunProgram 8 [3,3,1108,-1,8,3,4,3,99] `shouldBe` 1
        it "3,3,1108,-1,8,3,4,3,99 outputs False if input not equals 8" $
            outputOfRunProgram 9 [3,3,1108,-1,8,3,4,3,99] `shouldBe` 0
        it "3,3,1107,-1,8,3,4,3,99 outputs True if input < 8" $
            outputOfRunProgram (-4) [3,3,1107,-1,8,3,4,3,99] `shouldBe` 1
        it "3,3,1107,-1,8,3,4,3,99 outputs False if input >= 8" $
            outputOfRunProgram 100 [3,3,1107,-1,8,3,4,3,99] `shouldBe` 0
        it "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9 outputs 1 if input non-zero" $
            outputOfRunProgram (-1) [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] `shouldBe` 1
        it "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9 outputs 0 if input zero" $
            outputOfRunProgram 0 [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] `shouldBe` 0
        it "3,3,1105,-1,9,1101,0,0,12,4,12,99,1 outputs 1 if input non-zero" $
            outputOfRunProgram 1 [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] `shouldBe` 1
        it "3,3,1105,-1,9,1101,0,0,12,4,12,99,1 outputs 0 if input zero" $
            outputOfRunProgram 0 [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] `shouldBe` 0
        it "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99\
            \outputs 999 if input < 8" $
            outputOfRunProgram 7 [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99] 
            `shouldBe` 999
        it "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99\
            \outputs 1000 if input == 8" $
            outputOfRunProgram 8 [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99] 
            `shouldBe` 1000
        it "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99\
            \outputs 1001 if input > 8" $
            outputOfRunProgram 99 [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99] 
            `shouldBe` 1001
        describe "JumpIfTrue instr" $ do
            it "1005,0,6,4,9,99,4,10,99,42,25 should ouptput 25" $
                outputOfRunProgram 1 [1005,0,6,4,9,99,4,10,99,42,25] `shouldBe` 25
            it "1105,0,6,4,9,99,4,10,99,42,25 should ouptput 42" $
                outputOfRunProgram 1 [1105,0,6,4,9,99,4,10,99,42,25] `shouldBe` 42
        describe "JumpIfFalse instr" $ do
            it "1005,0,6,4,9,99,4,10,99,42,25 should ouptput 25" $
                outputOfRunProgram 1 [1006,0,6,4,9,99,4,10,99,42,25] `shouldBe` 42
            it "1105,0,6,4,9,99,4,10,99,42,25 should ouptput 42" $
                outputOfRunProgram 1 [1106,0,6,4,9,99,4,10,99,42,25] `shouldBe` 25
        describe "LessThan instr" $ do
            it "1107,0,1,5,104,-1,99 should output 1" $
                outputOfRunProgram 1 [1107,0,1,5,104,-1,99 ] `shouldBe` 1
            it "7,0,1,5,104,-1,99  should output 0" $
                outputOfRunProgram 1 [7,0,1,5,104,-1,99 ] `shouldBe` 0
        describe "Equals instr" $ do
            it "1108,3,5,5,104,-1,99 should output 0" $
                outputOfRunProgram 1 [1108,3,5,5,104,-1,99 ] `shouldBe` 0
            it "1008,3,5,5,104,-1,99  should output 1" $
                outputOfRunProgram 1 [1008,3,5,5,104,-1,99 ] `shouldBe` 1
    
    describe "Wires.extendPathSeg" $ do
        it "Extend newWire by U3 gives [(0,3)(0,0)]" $
            toCoords (extendPathSeg "U3" newWire) `shouldBe` [(0,3)]
        it "Extend newWire by L2 gives [(-2,0),(0,0)]" $
            toCoords (extendPathSeg "L2" newWire) `shouldBe` [(-2,0)]
    describe "Wires.tipDistance" $ do
        it "Wire extended by U3 should have tip distance 3" $
            tipDistance (extendPathSeg "U3" newWire) `shouldBe` 3
        it "Wire extended by L3,U4 should have tip distance 7" $
            tipDistance (fromPathStr "L3,U4") `shouldBe` 7
    describe "Wires.fromPath" $ do
        it "Path U100,L10,D40,R20 should have distance 70" $
            tipDistance (fromPathStr "U100,L10,D40,R20") `shouldBe` 70
    describe "Wires.smallestIntersectDistOfPaths" $ do
        it "is 159 for R75,D30,R83,U83,L12,D49,R71,U7,L72 and U62,R66,U55,R34,D71,R55,D58,R83" $
            smallestIntersectDistOfPaths "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83"
                `shouldBe` 159
        it "is 135 for R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51 and U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" $
            smallestIntersectDistOfPaths "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
                `shouldBe` 135

    describe "Wires.lengthToPoint" $ do
        it "should be Nothing when the point is off the path" $
            lengthToPoint (fromPathStr "L3,U4") (Pt 99 99) `shouldBe` Nothing
        it "should be 7 for L3,U4 when the point is the tip (ie -3, 4)" $
            lengthToPoint (fromPathStr "L3,U4") (Pt (-3) 4) `shouldBe` Just 7
        it "should be 5 for L3,U4 when the point is (-3, 2)" $
            lengthToPoint (fromPathStr "L3,U4") (Pt (-3) 2) `shouldBe` Just 5
    describe "Wires.lowestIntersectionLengthOfPaths" $ do
        it "is 610 for R75,D30,R83,U83,L12,D49,R71,U7,L72 and U62,R66,U55,R34,D71,R55,D58,R83" $
            lowestIntersectionLengthOfPaths "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83"
                `shouldBe` 610
        it "is 410 for R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51 and U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" $
            lowestIntersectionLengthOfPaths "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
                `shouldBe` 410

    describe "Passwords.isValid" $ do
        it "111111 is valid, assuming in range" $
            isValid 111111 `shouldBe` True
        it "223450 is not valid, because it has decreasing digits" $
            isValid 223450 `shouldBe` False
        it "123789 is not valid, because it has no doubles" $
            isValid 123789 `shouldBe` False
    describe "Passwords.isValidStrict" $ do
        it "112233 is valid, because all adjacent sets are length 2" $
            isValidStrict 112233 `shouldBe` True
        it "123444 is not valid, because 44 is part of a larger group" $
            isValidStrict 123444 `shouldBe` False
        it "111122 is valid, because it has still contains a double 2" $
            isValidStrict 111122 `shouldBe` True

    describe "Tree.mergeForest" $ do
        it "[(1,2),(3,4),(2,3)] should merge into one tree" $
            (length . mergeForest) [Node 1 [Node 2 []], Node 3 [Node 4 []], Node 2 [Node 3 []]] `shouldBe` 1
        it "[(2,3),(3,4),(1,2)] should merge into one tree" $
            (length . mergeForest) [Node 2 [Node 3 []], Node 3 [Node 4 []], Node 1 [Node 2 []]] `shouldBe` 1

    describe "Orbits.orbitOfStr" $ do
        it "AAA)BBB shoulbe become Node AAA [Node BBB []]" $
            orbitOfStr "AAA)BBB" `shouldBe` Node "AAA" [Node "BBB" []]
    describe "Orbits.totalDirectIndirectOrbits" $ do
        it "[COM)B,B)C,C)D,D)E,E)F,B)G,G)H,D)I,E)J,J)K,K)L] should be 42 total orbits" $
            totalDirectIndirectOrbits ["COM)B","B)C","C)D","D)E","E)F","B)G","G)H","D)I","E)J","J)K","K)L"] `shouldBe` 42
    describe "Orbits.countOrbitJumps" $ do
        it "COM)B,B)C,C)D,D)E,E)F,B)G,G)H,D)I,E)J,J)K,K)L,K)YOU,I)SAN should be 4 jumps" $
            countOrbitJumps "YOU" "SAN" ["COM)B","B)C","C)D","D)E","E)F","B)G","G)H","D)I","E)J","J)K","K)L","K)YOU","I)SAN"] `shouldBe` 4

        