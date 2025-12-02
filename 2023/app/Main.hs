module Main where

import           Data.List.Split    (splitOn)
import           Day1
import           Day7
import           Day8
import           Day9
import           System.Environment

main :: IO ()
main = do
    args <- getArgs
    let day = head args
    let part = args !! 1
    let file = args !! 2
    file <- lines <$> readFile file
    case day of
        "1" -> case part of
          "1" -> print $ Day1.part1 file
          "2" -> print $ Day1.part1 file
        "7" -> case part of
          "1" -> print $ playDay7Part1 file
          "2" -> print $ playDay7Part2 file
        "8" -> case part of
          "1" -> print $ playDay8Part1 file
          "2" -> print $ playDay8Part2 file
        "9" -> case part of
          "1" -> print $ playDay9Part1 file
          "2" -> print $ playDay9Part2 file
        _   -> putStrLn "No such day"

