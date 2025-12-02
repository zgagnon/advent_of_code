module Day8 where

import           Data.List                    (isSuffixOf)
import           Data.List.Split              (splitOn)
import           Data.List.Unique             (uniq)
import qualified Data.Map                     as Map
import           Debug.Trace
import           StringProcessing             (trim)
import           Text.ParserCombinators.ReadP (count)

playDay8Part1 :: [String] -> Int
playDay8Part1 (instructions:_:nodes) = let
        game = parseGameMap  nodes
    in countSteps game  instructions

playDay8Part2 :: [String] -> Int
playDay8Part2 (instructions:_:nodes) = let
        game = parseGameMap  nodes
    in countSteps2 game  instructions

type GameMap = Map.Map String (String, String)

parseGameMap :: [String] -> GameMap
parseGameMap input = let
    mapLines = do
        node <- input
        let clean = filter (not. flip elem " ()") node
        let id:rest = splitOn "=" clean
        let left:right:_ = splitOn "," (head rest)
        return (id, (left, right))
    in Map.fromList mapLines

countSteps :: GameMap ->  String -> Int
countSteps game instructions = fst $ go game 0 "AAA" (cycle instructions)
    where go  game currentCount firstNode (instruction:instructions)
            | firstNode == "ZZZ" = (currentCount, "ZZZ")
            | otherwise = let
                (left, right) = case Map.lookup firstNode game of
                    Just (l, r) -> (l, r)
                    Nothing     -> error "Node not found"
                nextNode = if instruction == 'L' then left else right
                in
                    --trace ("Found " ++ left ++ ", " ++ right ++ " " ++ [instruction] ++ " " ++ show currentCount)
                     go game (currentCount + 1) nextNode instructions

countStepsFrom :: GameMap -> Int -> String -> String -> Int
countStepsFrom game currentCount (instruction:instructions) firstNode
    | "Z" `isSuffixOf` firstNode = currentCount
    | otherwise = let
        (left, right) = case Map.lookup firstNode game of
            Just (l, r) -> (l, r)
            Nothing     -> error "Node not found"
        nextNode = if instruction == 'L' then left else right
        in countStepsFrom game (currentCount + 1) instructions nextNode

countSteps2 :: GameMap ->  String -> Int
countSteps2 game instructions = let
       starting = filter (isSuffixOf "A") (Map.keys game)
       path = map (countStepsFrom game 0 (cycle instructions))  starting
       in foldl lcm 1 path
