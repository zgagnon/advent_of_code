import           Data.Function      hiding (id)
import           Data.List          (intercalate)
import           Data.List.Split
import           Data.Map
import           Data.Set
import           Prelude            hiding (id)
import           StringProcessing

type GameMap = Map Int Int

main :: IO ()
main = do
    cards <- loadCards
    let pointsPerCard = Prelude.map (\card -> case length (wins card) of
            0 -> (0 :: Integer)
            x -> 2 ^ (x - 1)
            ) cards
    print $ sum pointsPerCard

    let gameMap = makeGameMap cards
    let freeGames = Prelude.foldl (countFreeGames (1 + length cards)) gameMap cards

    print $ Prelude.foldl (\acc (_,x) -> acc + x) 0 $ Data.Map.toList $ freeGames

loadCards :: IO [ScratchCard]
loadCards = do
    contents <-  lines <$> readFile "inputs/day-4.input"
    let cards = parseCards contents
    return $ findWins cards

makeGameMap :: [ScratchCard] -> GameMap
makeGameMap cards = Data.Map.fromList $ Prelude.map (\n -> (cardId n, 1)) cards

data ScratchCard = ScratchCard { cardId :: Int, winning :: Set String, have :: Set String, wins :: Set String} deriving Show

parseId :: String -> Int
parseId s =  splitOn " " s
            & last
            & read :: Int

parseCards :: [String] -> [ScratchCard]
parseCards contents = do
    (id:winning:have:_) <- splitOneOf ":|" <$> contents
    return ScratchCard {
        cardId = parseId id
        , winning = Data.Set.fromList $ splitOn " " $ trim winning
        , have = Data.Set.fromList $ splitOn " " $ trim have
        , wins = Data.Set.empty}

findWins :: [ScratchCard] -> [ScratchCard]
findWins cards = do
    card <- cards
    let wins = winning card `Data.Set.intersection` have card
    return card {wins = Data.Set.filter (/= "") wins}


countFreeGames :: Int -> GameMap -> ScratchCard -> GameMap
countFreeGames maxCard gameMap card =
    let
        winners = length $ Data.Set.map stringToInt $ wins $ head $ findWins [card]
        freeCardCount = gameMap ! cardId card
        index = cardId card + 1
        iterator = iterate (\(n, acc) ->
            if n > maxCard
            then (n + 1, acc)
            else (n + 1, Data.Map.insertWith (+) n freeCardCount acc)) (index, gameMap)
    in iterator !! winners & snd
