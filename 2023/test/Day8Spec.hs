module Day8Spec where
    
import qualified Data.Map   as Map
import           Day8       (countSteps, parseGameMap)
import           SpecHelper

spec :: Spec
spec = describe "Day8" $ do
    describe "parseGameMap" $ do
        it "Return a map with all nodes" $ do
            let input = ["AAA = (BBB, BBB)"
                    , "BBB = (AAA, ZZZ)"
                    , "ZZZ = (ZZZ, ZZZ)"]
            let expected = parseGameMap input
            expected `shouldBe` Map.fromList [("AAA", ("BBB", "BBB"))
                    , ("BBB", ("AAA", "ZZZ"))
                    , ("ZZZ", ("ZZZ", "ZZZ"))]
    describe "countSteps" $ do
        it "Return the number of steps to reach the end" $ do
            let input = ["AAA = (BBB, BBB)"
                    , "BBB = (AAA, ZZZ)"
                    , "ZZZ = (ZZZ, ZZZ)"]
            let game = parseGameMap input
            let instructions = "LLR"
            let expected = 6
            countSteps game  instructions `shouldBe` expected


