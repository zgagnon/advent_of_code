import Data.List.Split (splitOn)


main :: IO ()
main = do
    content <- lines <$> readFile "inputs/day-6.input"
    let races = parseRaces content
    let counts = do
            race <- races
            let plan = planRace race
            let count = length plan
            return count
    print $ product counts

stringToInt :: String -> Int
stringToInt s = read s :: Int

parseRaces :: [String] -> [(Int, Int)]
parseRaces races = let
        t = stringToInt $ concat $ tail $ filter (/= "") $ splitOn " " $ head races
        d = stringToInt $ concat $ tail $ filter (/= "") $ splitOn " " $ races !! 1
    in [(t, d)]

planRace :: (Int, Int) -> [Int]
planRace (t, r) = filter (> r) $ map (\n -> (t-n)*n) [0..t]