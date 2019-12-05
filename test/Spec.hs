import Test.Hspec
import Lib
import Intcode
import Wires
import Util

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
    describe "readPointerAtPointer" $ do
        it "1 of 2,3,0,3,99 is 3" $
            readPointerAtPointer [2,3,0,3,99] 1 `shouldBe` 3
        it "2 of 2,3,0,3,99 is 2" $
            readPointerAtPointer [2,3,0,3,99] 2 `shouldBe` 2
    describe "runIntcode" $ do
        it "1,0,0,0,99 becomes 2,0,0,0,99" $
            runIntcode [1,0,0,0,99] `shouldBe` [2,0,0,0,99]
        it "2,3,0,3,99 becomes 2,3,0,6,99" $
            runIntcode [2,3,0,3,99] `shouldBe` [2,3,0,6,99]
        it "2,4,4,5,99,0 becomes 2,4,4,5,99,9801" $
            runIntcode [2,4,4,5,99,0] `shouldBe` [2,4,4,5,99,9801]
        it "1,1,1,4,99,5,6,0,99 becomes 30,1,1,4,2,5,6,0,99" $
            runIntcode [1,1,1,4,99,5,6,0,99] `shouldBe` [30,1,1,4,2,5,6,0,99]
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
        