module Day9Spec where
import           Day9
import           SpecHelper

spec :: Spec
spec = describe "Day9" $ do
    describe "part 1" $ do
        describe "difference of sequence" $ do
            it "should return a sequence of differences" $ do
                let input = [1, 2, 3, 4, 5]
                let expected = [1, 1, 1, 1]
                differenceOfSequence input `shouldBe` expected
        describe "extrapolateSequence" $ do
            it "should extrapolate the next item in the sequnce" $ do
                let input = [1, 2, 3, 4, 5]
                let expected = 6
                extrapolateSequence input `shouldBe` expected
        describe "playPart1" $ do
            it "should sum the next item in several sequences" $ do
                let input = [
                        "0 3 6 9 12 15"
                        , "1 3 6 10 15 21"
                        , "10 13 16 21 30 45"
                        ]
                let expected = 114
                playDay9Part1 input `shouldBe` expected
    describe "part 2" $ do
        describe "backstrapolateSequence" $ do
            it "should backstrapolate the first item in the sequnce" $ do
                let input = [10,  13,  16,  21,  30,  45]
                let expected = 5
                backstrapolateSequence input `shouldBe` expected
