module StringProcessing 
( trim
, stringToInt
)
where

import Data.Char

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

stringToInt :: String -> Int
stringToInt s = read s :: Int