{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

import           Data.Function
import           Data.List        (isPrefixOf, sortBy,findIndex)
import           Data.List.Split
import           StringProcessing (stringToInt)
import Data.Foldable ( find )
import Debug.Trace

main :: IO ()
main = do
  content <- lines <$> readFile "inputs/day-5.input"
  let almanac = parseAlmanac content
  let r = rangeOf $ humidityToLocation almanac
  let location = findIndex (inSeedRange (seeds almanac) . locationToSeed almanac) r
  print "Location"
  print location
  print "done"

inSeedRange :: [(Int, Int)] -> Int -> Bool
inSeedRange almanac seed = case almanac of
  [] -> False
  [(start, leng)] -> seed >= start && seed < start + leng
  (start, leng):xs -> (seed >= start && seed < start + leng) || inSeedRange xs seed

rangeOf :: [Mapping] -> [Int]
rangeOf mapping = let
    sorted = sortBy (compare `on` destationStart)  mapping
  in foldl (\acc m  -> acc ++ [destationStart m..(destationStart m + rangeLength m)]) [] $ traceShowId sorted

data Mapping = Mapping {destationStart :: Int, sourceStart :: Int, rangeLength :: Int} deriving (Show)

data Almanac = Almanac {seeds :: [(Int, Int)]
, seedToSoil                  :: [Mapping]
, soilToFertilizer            :: [Mapping]
, fertilizerToWater           :: [Mapping]
, waterToLight                :: [Mapping]
, lightToTemperature          :: [Mapping]
, temperatureToHumidity       :: [Mapping]
, humidityToLocation          :: [Mapping]
} deriving (Show)

data SeedNeed = SeedNeed {seed :: Int
, soil :: Int
, fertilizer :: Int
, water :: Int
, light :: Int
, temperature :: Int
, humidity :: Int
, location::Int} deriving (Show)

parseAlmanac :: [String] -> Almanac
parseAlmanac file =
  let seedLine =  splitOn " " $ head file
      seedsTokens =  parseSeedLine $ tail seedLine
      seeds = traceShowId seedsTokens
      seedToSoil = getSection "seed-to-soil" file
      soilToFertilizer = getSection "soil-to-fertilizer" $ tail file
      fertilizerToWater = getSection "fertilizer-to-water" $ tail file
      waterToLight = getSection "water-to-light" $ tail file
      lightToTemperature = getSection "light-to-temperature" $ tail file
      temperatureToHumidity = getSection "temperature-to-humidity" $ tail file
      humidityToLocation = getSection "humidity-to-location" $ tail file
   in Almanac
        { seeds = seeds
          ,seedToSoil = case seedToSoil of
            Nothing -> error "No seed to soil section found"
            Just s  -> s
          ,soilToFertilizer = case soilToFertilizer of
            Nothing -> error "No soil to fertilizer section found"
            Just s  -> s
        ,fertilizerToWater = case fertilizerToWater of
            Nothing -> error "No fertilizer to water  section found"
            Just s  -> s
        ,waterToLight = case waterToLight of
            Nothing -> error "No water to light section found"
            Just s  -> s
        ,lightToTemperature = case lightToTemperature of
            Nothing -> error "No light to temperature section found"
            Just s  -> s
        ,temperatureToHumidity = case temperatureToHumidity of
            Nothing -> error "No temperature to humidity section found"
            Just s  -> s
        ,humidityToLocation = case humidityToLocation of
            Nothing -> error "No humidity to location section found"
            Just s  -> s
        }


parseSeedLine :: [String] -> [(Int, Int)]
parseSeedLine line = case line of
  [] -> []
  _ ->
    let
    first = stringToInt $ head line
    second = stringToInt $ line !! 1
    in (first, second) : parseSeedLine (drop 2 line)

getSection :: String -> [String] -> Maybe [Mapping]
getSection section list =
  let movedTo = dropWhile (not . isPrefixOf section) list
      rangeLines = case movedTo of
        [] -> Nothing
        _  -> Just $ takeWhile (/= "") $ tail movedTo
   in case rangeLines of
        Nothing -> Nothing
        Just l -> Just $ do
            mapLine <-  l
            let digits = map stringToInt $ splitOn " " mapLine
            return $ Mapping (head digits) (digits !! 1) (digits !! 2)

convert :: [Mapping] -> Int -> Int
convert mappings value = let
  mappingLine = find (\d -> sourceStart d <= value && value < sourceStart d + rangeLength d) mappings
  in case  mappingLine of
    --Nothing -> trace (show mappings ++ " converting " ++ show value ++ " to " ++ show value ) value
    --Just m -> trace (show mappings ++ "converting " ++ show value ++ " to " ++ show (destationStart m + (value - sourceStart m)) ) destationStart m + (value - sourceStart m)
    Nothing -> value
    Just m ->  destationStart m + (value - sourceStart m)

deconvert :: [Mapping] -> Int -> Int
deconvert mappings value = let
  mappingLine = find (\d -> destationStart d <= value && value < destationStart d + rangeLength d) mappings
  in case  mappingLine of
    Nothing -> value
    Just m ->  sourceStart m + (value - destationStart m)

seedToLocation :: Almanac -> Int -> Int
seedToLocation almanac seed = let
  soil = convert (seedToSoil almanac) seed
  fertilizer =  convert (soilToFertilizer almanac) soil
  water = convert (fertilizerToWater almanac) fertilizer
  light = convert (waterToLight almanac) water
  temperature = convert (lightToTemperature almanac) light
  humidity = convert (temperatureToHumidity almanac) temperature
  location = convert (humidityToLocation almanac) humidity
  in location

locationToSeed :: Almanac -> Int -> Int
locationToSeed almanac location = let
  humidity = deconvert (humidityToLocation almanac) location
  temperature = deconvert (temperatureToHumidity almanac) humidity
  light = deconvert (lightToTemperature almanac) temperature
  water = deconvert (waterToLight almanac) light
  fertilizer = deconvert (fertilizerToWater almanac) water
  soil = deconvert (soilToFertilizer almanac) fertilizer
  seed = deconvert (seedToSoil almanac) soil
  in seed
