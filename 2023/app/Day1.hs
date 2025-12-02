module Day1 where

import           Data.Char       (isDigit)
import           Data.Foldable
import           Data.Function
import           Data.List       (isPrefixOf, isSuffixOf)
import           Data.List.Split
import           Day7            (stringToInt)
import           Debug.Trace
import           System.IO


front :: String -> Int
front x
  | "one" `isPrefixOf` x = 1
  | "two" `isPrefixOf` x = 2
  | "three" `isPrefixOf` x = 3
  | "four" `isPrefixOf` x = 4
  | "five" `isPrefixOf` x = 5
  | "six" `isPrefixOf` x = 6
  | "seven" `isPrefixOf` x = 7
  | "eight" `isPrefixOf` x = 8
  | "nine" `isPrefixOf` x = 9
  |  isDigit (head x) = stringToInt (take 1 x)
  | otherwise = front (tail x)

back :: String -> Int
back x
  | "one" `isSuffixOf` x = 1
  | "two" `isSuffixOf` x = 2
  | "three" `isSuffixOf` x = 3
  | "four" `isSuffixOf` x = 4
  | "five" `isSuffixOf` x = 5
  | "six" `isSuffixOf` x = 6
  | "seven" `isSuffixOf` x = 7
  | "eight" `isSuffixOf` x = 8
  | "nine" `isSuffixOf` x = 9
  |  isDigit (last x) = stringToInt (take 1 (reverse x))
  | otherwise = back (init x)

dumbpair :: String -> Int
dumbpair x = let
  f = front x
  b = back x
  in trace (x ++ " " ++ show f ++ " " ++ show b) (f * 10 + b)

part1 :: [String] -> Int
part1 input =  let
  parsed = map dumbpair input
  sums = sum parsed
  in sums

main :: IO ()
main = do
  contents <- readFile "./inputs/day-1.input"
  let split = splitOn "\n" contents
  let parsed = map dumbpair split
  let sums = sum parsed
  print parsed
  print (length parsed)
  print sums
