import           Data.Char          (isSpace)
import           Data.List.Split
import           Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
    contents <- lines <$> readFile "inputs/day-2.input"
    let games = map parseGame contents
    let possibleIds = sum $ map gameId $ filter (possibleWithBag maximumPull) games
    print "Part 1, sum of id of possible games"
    pPrint possibleIds
    let minimums = map minimumBag games
    print "Part 2, sum of bag power of minimum bags"
    pPrint $ sum $ map bagPower minimums



data Pull = Pull { red::Int, green::Int, blue::Int} deriving Show
data Game = Game { gameId :: Int, pulls:: [Pull]} deriving Show

stringToInt :: String -> Int
stringToInt s = read s :: Int

maximumPull :: Pull
maximumPull = Pull {red=12, green=13, blue=14}

parseGame :: String -> Game
parseGame s = let
    gameAndPulls = splitOn ":" s
    gameId = stringToInt $ head $ tail $ splitOn " " $ head gameAndPulls
    pulls = map parsePull $ splitOn ";" $ last gameAndPulls
    in Game {gameId=gameId, pulls=  pulls}

parsePull :: String -> Pull
parsePull s = foldl partToPull
            Pull {red=0, green=0, blue=0} $ map (splitOn " ")
            $ map (dropWhile isSpace) $ splitOn "," s

partToPull :: Pull -> [String] -> Pull
partToPull acc next = case next of
    [count,"red"]   -> acc {red = read count :: Int}
    [count,"green"] -> acc {green = read count :: Int}
    [count,"blue"]  -> acc {blue = read count :: Int}
    _           -> acc

possibleWithBag ::  Pull -> Game -> Bool
possibleWithBag bag game = foldl (\acc next ->
    (red next) <= (red bag)
    && (green next) <= (green bag)
    && (blue next) <= (blue bag) && acc) True $ pulls game

minimumBag :: Game -> Pull
minimumBag game = foldl (\acc next ->
    next {red = max (red acc) (red next),
    green = max (green acc) (green next),
    blue = max (blue acc) (blue next)})
    Pull {red=0, green=0, blue=0} $ pulls game

bagPower :: Pull -> Int
bagPower bag = (red bag) * (green bag) * (blue bag)
