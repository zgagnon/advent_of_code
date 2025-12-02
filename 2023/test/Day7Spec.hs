module Day7Spec where

import           Data.List.Split (splitOn)
import           Day7
import           SpecHelper
import           Test.Hspec

spec :: Spec
spec = describe "Advent of code day 7" $ do
  describe "matchHandType" $ do
    describe "when given a hand with 5 different cards" $ do
      it "returns High" $ do
        matchHandType "12345" `shouldBe` High
    describe "when given a hand with one pair" $ do
      it "returns One" $ do
        matchHandType "11234" `shouldBe` One
    describe "when given a hand with two pairs" $ do
      it "returns Two" $ do
        matchHandType "11223" `shouldBe` Two
    describe "when given a hand with three of a kind" $ do
      it "returns Three" $ do
        matchHandType "11123" `shouldBe` Three
    describe "when given a hand with a full house" $ do
      it "returns Full" $ do
        matchHandType "11122" `shouldBe` Full
    describe "when given a hand with four of a kind" $ do
      it "returns Four" $ do
        matchHandType "11112" `shouldBe` Four
    describe "when given a hand with five of a kind" $ do
      it "returns Five" $ do
        matchHandType "11111" `shouldBe` Five
  describe "compareHands" $ do
    describe "when given two hands with different types" $ do
      describe "high is LT one" $ do
        it "returns the hand with the highest type" $ do
          let hand1 = Hand {handType = High, hand = "12345", bid = 1}
          let hand2 = Hand {handType = One, hand = "11234", bid = 1}
          compareHands compareFaceCards hand1 hand2 `shouldBe` LT
      describe "one is LT two" $ do
        it "returns the hand with the highest type" $ do
          let hand1 = Hand {handType = One, hand = "11234", bid = 1}
          let hand2 = Hand {handType = Two, hand = "11223", bid = 1}
          compareHands compareFaceCards hand1 hand2 `shouldBe` LT
      describe "two is LT three" $ do
        it "returns the hand with the highest type" $ do
          let hand1 = Hand {handType = Two, hand = "11223", bid = 1}
          let hand2 = Hand {handType = Three, hand = "11123", bid = 1}
          compareHands compareFaceCards hand1 hand2 `shouldBe` LT
      describe "three is LT full" $ do
        it "returns the hand with the highest type" $ do
          let hand1 = Hand {handType = Three, hand = "11123", bid = 1}
          let hand2 = Hand {handType = Full, hand = "11122", bid = 1}
          compareHands compareFaceCards hand1 hand2 `shouldBe` LT
      describe "full is LT four" $ do
        it "returns the hand with the highest type" $ do
          let hand1 = Hand {handType = Full, hand = "11122", bid = 1}
          let hand2 = Hand {handType = Four, hand = "11112", bid = 1}
          compareHands compareFaceCards hand1 hand2 `shouldBe` LT
      describe "four is LT five" $ do
        it "returns the hand with the highest type" $ do
          let hand1 = Hand {handType = Four, hand = "11112", bid = 1}
          let hand2 = Hand {handType = Five, hand = "11111", bid = 1}
          compareHands compareFaceCards hand1 hand2 `shouldBe` LT
    describe "when given two hands with the same type" $ do
      describe "when the first card is different" $ do
        describe "when the first cards are both numbers" $ do
          it "returns the hand with the highest card" $ do
            let hand1 = Hand {handType = High, hand = "22345", bid = 1}
            let hand2 = Hand {handType = High, hand = "12346", bid = 1}
            compareHands compareFaceCards hand1 hand2 `shouldBe` GT
        describe "when the first card is a letter and the other is a number" $ do
          it "returns the hand with the highest card" $ do
            let hand1 = Hand {handType = High, hand = "A2345", bid = 1}
            let hand2 = Hand {handType = High, hand = "12346", bid = 1}
            compareHands compareFaceCards hand1 hand2 `shouldBe` GT
        describe "when the first card is a digit and the other one is a letter" $ do
          it "returns the hand with the highest card" $ do
            let hand1 = Hand {handType = High, hand = "12346", bid = 1}
            let hand2 = Hand {handType = High, hand = "A2345", bid = 1}
            compareHands compareFaceCards hand1 hand2 `shouldBe` LT
        describe "when the first card is a letter and the other is a letter" $ do
          describe "when the first card is an ace" $ do
            it "returns the hand with the highest card" $ do
              let hand1 = Hand {handType = High, hand = "A2345", bid = 1}
              let hand2 = Hand {handType = High, hand = "K2346", bid = 1}
              compareHands compareFaceCards hand1 hand2 `shouldBe` GT
          describe "when the second card is an ace" $ do
            it "returns the hand with the highest card" $ do
              let hand1 = Hand {handType = High, hand = "K2346", bid = 1}
              let hand2 = Hand {handType = High, hand = "A2345", bid = 1}
              compareHands compareFaceCards hand1 hand2 `shouldBe` LT
          describe "when the first card is a king" $ do
            it "returns the hand with the highest card" $ do
              let hand1 = Hand {handType = High, hand = "K2346", bid = 1}
              let hand2 = Hand {handType = High, hand = "Q2346", bid = 1}
              compareHands compareFaceCards hand1 hand2 `shouldBe` GT
          describe "when the second card is a king" $ do
            it "returns the hand with the highest card" $ do
              let hand1 = Hand {handType = High, hand = "Q2346", bid = 1}
              let hand2 = Hand {handType = High, hand = "K2346", bid = 1}
              compareHands compareFaceCards hand1 hand2 `shouldBe` LT
          describe "when the first card is a queen" $ do
            it "returns the hand with the highest card" $ do
              let hand1 = Hand {handType = High, hand = "Q2346", bid = 1}
              let hand2 = Hand {handType = High, hand = "J2346", bid = 1}
              compareHands compareFaceCards hand1 hand2 `shouldBe` GT
          describe "when the second card is a queen" $ do
            it "returns the hand with the highest card" $ do
              let hand1 = Hand {handType = High, hand = "J2346", bid = 1}
              let hand2 = Hand {handType = High, hand = "Q2346", bid = 1}
              compareHands compareFaceCards hand1 hand2 `shouldBe` LT
          describe "when the first card is a jack" $ do
            it "returns the hand with the highest card" $ do
              let hand1 = Hand {handType = High, hand = "J2346", bid = 1}
              let hand2 = Hand {handType = High, hand = "T2346", bid = 1}
              compareHands compareFaceCards hand1 hand2 `shouldBe` GT
          describe "when the second card is a jack" $ do
            it "returns the hand with the highest card" $ do
              let hand1 = Hand {handType = High, hand = "T2346", bid = 1}
              let hand2 = Hand {handType = High, hand = "J2346", bid = 1}
              compareHands compareFaceCards hand1 hand2 `shouldBe` LT
  describe "rank hands" $ do
    it "returns the hands ranked" $ do
      let hands = [Hand {handType = Four, hand = "11112", bid = 1}
            , Hand {handType = High, hand = "12345", bid = 1}
            , Hand {handType = One, hand = "11234", bid = 1}
            , Hand {handType = Two, hand = "11223", bid = 1}]
      let rankedHands = rankHands compareFaceCards hands
      rankedHands `shouldBe` [(1, Hand {handType = High, hand = "12345", bid = 1})
        , (2, Hand {handType = One, hand = "11234", bid = 1})
        , (3,Hand {handType = Two, hand = "11223", bid = 1})
        , (4, Hand {handType = Four, hand = "11112", bid = 1})]
  describe "scoring the ranked hands" $ do
    it "returns the total" $ do
      let rankedHands = [(1, Hand {handType = One, hand = "32T3K", bid = 765})
            , (2, Hand {handType = Two, hand = "KTJJT", bid = 220})
            , (3,Hand {handType = Two, hand = "KK677", bid = 28})
            , (4, Hand {handType = Three, hand = "T55J5", bid = 684})
            , (5, Hand {handType = Three, hand = "QQQJA", bid = 483})]
      let total = scoreRankedHands rankedHands
      total `shouldBe` 6440
  describe "parsing the file" $ do
    it "returns the hands" $ do
      let lines = splitOn "\n" "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483"
      let hands = parseHands matchHandType lines
      hands `shouldBe` [
              Hand {handType = One, hand = "32T3K", bid = 765}
              , Hand {handType = Three, hand = "T55J5", bid = 684}
              , Hand {handType = Two, hand = "KK677", bid = 28}
              , Hand {handType = Two, hand = "KTJJT", bid = 220}
              , Hand {handType = Three, hand = "QQQJA", bid = 483}
              ]
  describe "play part 1"  $ do
    it "returns the total" $ do
      let total = playDay7Part1 $ splitOn "\n" "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483"
      total `shouldBe` 6440
  describe "matchHandsWithJoker" $ do
    describe "when the hand is all jokers" $ do
      it "returns Five" $ do
        matchHandsWithJoker "JJJJJ" `shouldBe` Five
    describe "when given a hand with 5 different cards" $ do
      it "returns High" $ do
        matchHandsWithJoker "12345" `shouldBe` High
    describe "when given a hand with one pair" $ do
      it "returns One" $ do
        matchHandsWithJoker "1123T" `shouldBe` One
    describe "when given a hand with two pairs" $ do
      it "returns Two" $ do
        matchHandsWithJoker "1122T" `shouldBe` Two
    describe "when given a hand with three of a kind" $ do
      it "returns Three" $ do
        matchHandsWithJoker "1112T" `shouldBe` Three
    describe "when given a hand with a full house" $ do
      it "returns Full" $ do
        matchHandsWithJoker "111TT" `shouldBe` Full
    describe "when given a hand with four of a kind" $ do
      it "returns Four" $ do
        matchHandsWithJoker "1111T" `shouldBe` Four
    describe "when given a hand with five of a kind" $ do
      it "returns Five" $ do
        matchHandsWithJoker "11111" `shouldBe` Five
    describe "when a no-match contains a joker" $ do
      it "returns a One" $ do
        matchHandsWithJoker "1234J" `shouldBe` One
    describe "when a one-match contains a joker" $ do
      it "returns a Three" $ do
        matchHandsWithJoker "1123J" `shouldBe` Three
    describe "when a two-match contains a joker" $ do
      it "returns a Full" $ do
        matchHandsWithJoker "1122J" `shouldBe` Full
    describe "when a three-match contains a joker" $ do
      it "returns a Four" $ do
        matchHandsWithJoker "1112J" `shouldBe` Four
    describe "when a four-match contains a joker" $ do
      it "returns a Five" $ do
        matchHandsWithJoker "1111J" `shouldBe` Five
    describe "when there are two jokers" $ do
      it "1 becomes 3" $ do
        matchHandsWithJoker "152JJ" `shouldBe` Three
      it "2 becomes 4" $ do
        matchHandsWithJoker "115JJ" `shouldBe` Four
      it "3 becomes 5" $ do
        matchHandsWithJoker "111JJ" `shouldBe` Five
    describe "when there are three joker" $ do
      it "1 becomes 4" $ do
        matchHandsWithJoker "15JJJ" `shouldBe` Four
      it "2 becomes 5" $ do
        matchHandsWithJoker "11JJJ" `shouldBe` Five
  describe "compareFaceCardsWithJoker" $ do
    describe "when given two hands with different types" $ do
      describe "high is LT one" $ do
        it "returns the hand with the highest type" $ do
          let hand1 = Hand {handType = High, hand = "12345", bid = 1}
          let hand2 = Hand {handType = One, hand = "11234", bid = 1}
          compareHandsWithJoker hand1 hand2 `shouldBe` LT
      describe "one is LT two" $ do
        it "returns the hand with the highest type" $ do
          let hand1 = Hand {handType = One, hand = "11234", bid = 1}
          let hand2 = Hand {handType = Two, hand = "11223", bid = 1}
          compareHandsWithJoker hand1 hand2 `shouldBe` LT
      describe "two is LT three" $ do
        it "returns the hand with the highest type" $ do
          let hand1 = Hand {handType = Two, hand = "11223", bid = 1}
          let hand2 = Hand {handType = Three, hand = "11123", bid = 1}
          compareHandsWithJoker hand1 hand2 `shouldBe` LT
      describe "three is LT full" $ do
        it "returns the hand with the highest type" $ do
          let hand1 = Hand {handType = Three, hand = "11123", bid = 1}
          let hand2 = Hand {handType = Full, hand = "11122", bid = 1}
          compareHandsWithJoker hand1 hand2 `shouldBe` LT
      describe "full is LT four" $ do
        it "returns the hand with the highest type" $ do
          let hand1 = Hand {handType = Full, hand = "11122", bid = 1}
          let hand2 = Hand {handType = Four, hand = "11112", bid = 1}
          compareHandsWithJoker hand1 hand2 `shouldBe` LT
      describe "four is LT five" $ do
        it "returns the hand with the highest type" $ do
          let hand1 = Hand {handType = Four, hand = "11112", bid = 1}
          let hand2 = Hand {handType = Five, hand = "11111", bid = 1}
          compareHandsWithJoker hand1 hand2 `shouldBe` LT
    describe "when given two hands with the same type" $ do
      describe "when the first card is different" $ do
        describe "when the first cards are both Jokers" $ do
          it "return EQ" $ do
            let hand1 = Hand {handType = High, hand = "J2345", bid = 1}
            let hand2 = Hand {handType = High, hand = "J2345", bid = 1}
            compareHandsWithJoker hand1 hand2 `shouldBe` EQ
        describe "when the left first card if a joker" $ do
          it "returns the hand with the highest card" $ do
            let hand1 = Hand {handType = High, hand = "J2345", bid = 1}
            let hand2 = Hand {handType = High, hand = "12346", bid = 1}
            compareHandsWithJoker hand1 hand2 `shouldBe` LT
        describe "when the right first card if a joker" $ do
          it "returns the hand with the highest card" $ do
            let hand1 = Hand {handType = High, hand = "12346", bid = 1}
            let hand2 = Hand {handType = High, hand = "J2345", bid = 1}
            compareHandsWithJoker hand1 hand2 `shouldBe` GT
        describe "when the first cards are both numbers" $ do
          it "returns the hand with the highest card" $ do
            let hand1 = Hand {handType = High, hand = "22345", bid = 1}
            let hand2 = Hand {handType = High, hand = "12346", bid = 1}
            compareHandsWithJoker hand1 hand2 `shouldBe` GT
        describe "when the first card is a letter and the other is a number" $ do
          it "returns the hand with the highest card" $ do
            let hand1 = Hand {handType = High, hand = "A2345", bid = 1}
            let hand2 = Hand {handType = High, hand = "12346", bid = 1}
            compareHandsWithJoker hand1 hand2 `shouldBe` GT
        describe "when the first card is a digit and the other one is a letter" $ do
          it "returns the hand with the highest card" $ do
            let hand1 = Hand {handType = High, hand = "12346", bid = 1}
            let hand2 = Hand {handType = High, hand = "A2345", bid = 1}
            compareHandsWithJoker hand1 hand2 `shouldBe` LT
        describe "when the first card is a letter and the other is a letter" $ do
          describe "when the first card is an ace" $ do
            it "returns the hand with the highest card" $ do
              let hand1 = Hand {handType = High, hand = "A2345", bid = 1}
              let hand2 = Hand {handType = High, hand = "K2346", bid = 1}
              compareHandsWithJoker hand1 hand2 `shouldBe` GT
          describe "when the second card is an ace" $ do
            it "returns the hand with the highest card" $ do
              let hand1 = Hand {handType = High, hand = "K2346", bid = 1}
              let hand2 = Hand {handType = High, hand = "A2345", bid = 1}
              compareHandsWithJoker hand1 hand2 `shouldBe` LT
          describe "when the first card is a king" $ do
            it "returns the hand with the highest card" $ do
              let hand1 = Hand {handType = High, hand = "K2346", bid = 1}
              let hand2 = Hand {handType = High, hand = "Q2346", bid = 1}
              compareHandsWithJoker hand1 hand2 `shouldBe` GT
          describe "when the second card is a king" $ do
            it "returns the hand with the highest card" $ do
              let hand1 = Hand {handType = High, hand = "Q2346", bid = 1}
              let hand2 = Hand {handType = High, hand = "K2346", bid = 1}
              compareHandsWithJoker hand1 hand2 `shouldBe` LT
          describe "when the first card is a queen" $ do
            it "returns the hand with the highest card" $ do
              let hand1 = Hand {handType = High, hand = "Q2346", bid = 1}
              let hand2 = Hand {handType = High, hand = "J2346", bid = 1}
              compareHandsWithJoker hand1 hand2 `shouldBe` GT
          describe "when the second card is a queen" $ do
            it "returns the hand with the highest card" $ do
              let hand1 = Hand {handType = High, hand = "J2346", bid = 1}
              let hand2 = Hand {handType = High, hand = "Q2346", bid = 1}
              compareHandsWithJoker hand1 hand2 `shouldBe` LT
  describe "play part 2"  $ do
     it "returns the total" $ do
      let total = playDay7Part2 $ splitOn "\n" "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483"
      total `shouldBe` 5905
