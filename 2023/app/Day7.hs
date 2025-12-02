module Day7 where

import           Data.Char       (isDigit)
import           Data.List       (sortBy)
import           Data.List.Split (splitOn)
import           Data.Map        (elems, empty, insertWith)
import           Data.Ord        (Down (..), comparing)
import           Debug.Trace

data HandType = High | One | Two | Three | Full | Four | Five deriving (Show, Eq, Ord)

data Hand = Hand {
    handType :: HandType
    ,hand    :: String
    ,bid     :: Int
} deriving (Show, Eq, Ord)

playDay7Part2 :: [String] -> Int
playDay7Part2 input = let
    hands = parseHands matchHandsWithJoker input
    rankedHands = rankHands compareFaceCardsWithJoker hands
    in scoreRankedHands rankedHands

playDay7Part1 :: [String] -> Int
playDay7Part1 input = let
    hands = parseHands matchHandType input
    rankedHands = rankHands compareFaceCards hands
    in scoreRankedHands rankedHands

parseHands :: (String -> HandType) -> [String] -> [Hand]
parseHands matchHands hands = do
    hand <- hands
    let parts = splitOn " " hand
    let handType = matchHands $ head parts
    return $ Hand {
        hand = head parts
        , bid = stringToInt $  parts !! 1
        , handType = handType
    }

matchHandType :: String -> HandType
matchHandType hand = let
    count = foldl (\acc x -> insertWith (+) x 1 acc) empty hand
    cardCounts :: [Int] = sortBy (comparing Data.Ord.Down) (elems count)
    in case  cardCounts of
        5:_   -> Five
        4:_   -> Four
        3:2:_ -> Full
        3:_   -> Three
        2:2:_ -> Two
        2:_   -> One
        _     -> High

matchHandsWithJoker :: String -> HandType
matchHandsWithJoker hand = let
    jokerCount = length $ filter (== 'J') hand
    in if jokerCount == 5
        then Five
        else let
            handWithoutJoker = filter (/= 'J') hand
            count = foldl (\acc x -> insertWith (+) x 1 acc) empty handWithoutJoker
            cardCounts :: [Int] = sortBy (comparing Data.Ord.Down) (elems count)
            countsWithJoker = (jokerCount + head cardCounts):tail cardCounts
            in case  countsWithJoker of
                5:_   -> Five
                4:_   -> Four
                3:2:_ -> Full
                3:_   -> Three
                2:2:_ -> Two
                2:_   -> One
                _     -> High

stringToInt :: String -> Int
stringToInt s = read s :: Int

compareHands :: (Char -> Char -> Ordering) -> Hand -> Hand -> Ordering
compareHands faceCompare hand1 hand2 = let
    handType1 = handType hand1
    handType2 = handType hand2
    in case compare handType1 handType2 of
        EQ -> compareHandValues faceCompare (hand hand1) (hand hand2)
        x  -> x

compareHandValues ::  (Char -> Char -> Ordering) -> String -> String -> Ordering
compareHandValues faceCompare left right
  | null left && null right = EQ
  | head left == head right = compareHandValues faceCompare (tail left) (tail right)
  | otherwise = faceCompare (head left) (head right)

compareFaceCards :: Char -> Char -> Ordering
compareFaceCards x y
    | isDigit x && isDigit y = compare x y
    | isDigit x = LT
    | isDigit y = GT
    | x == y = EQ
    | x == 'A' = GT
    | y == 'A' = LT
    | x == 'K' = GT
    | y == 'K' = LT
    | x == 'Q' = GT
    | y == 'Q' = LT
    | x == 'J' = GT
    | otherwise = LT

rankHands :: (Char -> Char -> Ordering) -> [Hand] -> [(Int, Hand)]
rankHands faceCompare hands =  zip [1..] $ sortBy (compareHands faceCompare) hands

scoreRankedHands :: [(Int, Hand)] -> Int
scoreRankedHands rankedHands = sum $ map (\(rank, hand) -> rank * bid hand) rankedHands

compareHandsWithJoker :: Hand -> Hand -> Ordering
compareHandsWithJoker hand1 hand2 = let
    handType1 = handType hand1
    handType2 = handType hand2
    in case compare handType1 handType2 of
        EQ -> compareHandValues compareFaceCardsWithJoker (hand hand1) (hand hand2)
        x  -> x

compareFaceCardsWithJoker :: Char -> Char -> Ordering
compareFaceCardsWithJoker x y
    | x == 'J' && y == 'J' = EQ
    | x == 'J' = LT
    | y == 'J' = GT
    | isDigit x && isDigit y = compare x y
    | isDigit x = LT
    | isDigit y = GT
    | x == y = EQ
    | x == 'A' = GT
    | y == 'A' = LT
    | x == 'K' = GT
    | y == 'K' = LT
    | x == 'Q' = GT
    | y == 'Q' = LT
    | otherwise = LT
