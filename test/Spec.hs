import Test.Hspec
import Lib
import Intcode

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
    describe "readPosOfPos" $ do
        it "1 of 2,3,0,3,99 is 3" $
            readPosOfPos [2,3,0,3,99] 1 `shouldBe` 3
        it "2 of 2,3,0,3,99 is 2" $
            readPosOfPos [2,3,0,3,99] 2 `shouldBe` 2
        
    describe "intcodes1" $ do
        it "1,0,0,0,99 becomes 2,0,0,0,99" $
            intcodes1 [1,0,0,0,99] `shouldBe` [2,0,0,0,99]
        it "2,3,0,3,99 becomes 2,3,0,6,99" $
            intcodes1 [2,3,0,3,99] `shouldBe` [2,3,0,6,99]
        it "2,4,4,5,99,0 becomes 2,4,4,5,99,9801" $
            intcodes1 [2,4,4,5,99,0] `shouldBe` [2,4,4,5,99,9801]
        it "1,1,1,4,99,5,6,0,99 becomes 30,1,1,4,2,5,6,0,99" $
            intcodes1 [1,1,1,4,99,5,6,0,99] `shouldBe` [30,1,1,4,2,5,6,0,99]

    describe "runIntcode" $ do
        it "1,0,0,0,99 becomes 2,0,0,0,99" $
            runIntcode [1,0,0,0,99] `shouldBe` [2,0,0,0,99]
        it "2,3,0,3,99 becomes 2,3,0,6,99" $
            runIntcode [2,3,0,3,99] `shouldBe` [2,3,0,6,99]
        it "2,4,4,5,99,0 becomes 2,4,4,5,99,9801" $
            runIntcode [2,4,4,5,99,0] `shouldBe` [2,4,4,5,99,9801]
        it "1,1,1,4,99,5,6,0,99 becomes 30,1,1,4,2,5,6,0,99" $
            runIntcode [1,1,1,4,99,5,6,0,99] `shouldBe` [30,1,1,4,2,5,6,0,99]