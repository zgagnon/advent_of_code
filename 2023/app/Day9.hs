module Day9 where
import           Data.IntMap      (difference)
import           Day1             (back)
import           StringProcessing (stringToInt)

playDay9Part1 :: [String] -> Int
playDay9Part1 readingSets = let
    sequences = do
        readingSet <- readingSets
        let numbers = map stringToInt $ words readingSet
        return numbers
    in sum $ map extrapolateSequence sequences


playDay9Part2 :: [String] -> Int
playDay9Part2 readingSets = let
    sequences = do
        readingSet <- readingSets
        let numbers = map stringToInt $ words readingSet
        return numbers
    in sum $ map backstrapolateSequence sequences

differenceOfSequence :: [Int] -> [Int]
differenceOfSequence list = case list of
    []       -> []
    [x]      -> []
    (x:y:xs) -> (y - x) : differenceOfSequence (y:xs)

extrapolateSequence :: [Int] -> Int
extrapolateSequence list
    | all (== 0) list = 0
    | otherwise = let
        nextSequence = differenceOfSequence list
        nextItem = extrapolateSequence nextSequence
        thisItem = last list
    in nextItem + thisItem

backstrapolateSequence :: [Int] -> Int
backstrapolateSequence list
    | all (== 0) list = 0
    | otherwise = let
        nextSequence = differenceOfSequence list
        previousItem = backstrapolateSequence nextSequence
        thisItem = head list
    in thisItem - previousItem
