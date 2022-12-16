module AdventOfCode.Year2022.Day15 where

import AdventOfCode.Fixture (runChallenge)
import Control.Lens
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.List (find)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Linear (V2)
import Linear.V2 (V2 (V2))
import Text.Parsec (sepEndBy1, string)
import Text.Parsec.Char (char)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim (parse)
import Text.Parsec.String (Parser)
import Util (parseSNumber)

data Measurement = Measurement {_sensorPos :: V2 Int, _beaconPos :: V2 Int}
  deriving (Show)

makeLenses ''Measurement

taxicabDistance :: V2 Int -> V2 Int -> Int
taxicabDistance (V2 x1 y1) (V2 x2 y2) = abs (x1 - x2) + abs (y1 - y2)

taxicabRadius :: Measurement -> Int
taxicabRadius m = taxicabDistance (m ^. sensorPos) (m ^. beaconPos)

data SensorSet = SensorSet
  { _sensors :: [(V2 Int, Int)],
    _beacons :: HashSet (V2 Int)
  }
  deriving (Show)

makeLenses ''SensorSet

empty :: SensorSet
empty = SensorSet {_sensors = [], _beacons = HS.empty}

getMinX :: SensorSet -> Int
getMinX = minimum . fmap (\(V2 x _, r) -> x - r) . _sensors

getMaxX :: SensorSet -> Int
getMaxX = maximum . fmap (\(V2 x _, r) -> x + r) . _sensors

addMeasurement :: Measurement -> SensorSet -> SensorSet
addMeasurement m v =
  let sensor = m ^. sensorPos
      sensorRadius = taxicabRadius m
      beacon = m ^. beaconPos
   in (v & sensors %~ ((sensor, sensorRadius) :)) & beacons %~ HS.insert beacon

fromMeasurements :: [Measurement] -> SensorSet
fromMeasurements = foldr addMeasurement empty

isInRadius :: (V2 Int, Int) -> V2 Int -> Bool
isInRadius (center, dist) toCheck = taxicabDistance center toCheck <= dist

can'tContainBeacon :: SensorSet -> V2 Int -> Bool
can'tContainBeacon v pos
  | HS.member pos (v ^. beacons) = False
  | otherwise = any (`isInRadius` pos) (v ^. sensors)

positionParser :: Parser (V2 Int)
positionParser = do
  string "x="
  x <- parseSNumber
  string ", y="
  V2 x <$> parseSNumber

measurementParser :: Parser Measurement
measurementParser = do
  string "Sensor at "
  sensorPos' <- positionParser
  string ": closest beacon is at "
  beaconPos' <- positionParser
  return $ Measurement {_sensorPos = sensorPos', _beaconPos = beaconPos'}

inputParser :: Parser [Measurement]
inputParser = measurementParser `sepEndBy1` char '\n'

parseInput :: String -> Either ParseError [Measurement]
parseInput = parse inputParser ""

positionsOfRadius :: V2 Int -> Int -> Seq (V2 Int)
positionsOfRadius center radius = fmap (+ center) $ foldr (<>) Seq.empty $ fmap (\f -> fmap (f . (\x -> V2 x (radius - x))) (Seq.fromList [0 .. radius])) [id, \(V2 x y) -> V2 x (-y), \(V2 x y) -> V2 (-x) y, negate]

part1 :: [Measurement] -> Int
part1 input =
  let sset = fromMeasurements input
      min' = getMinX sset
      max' = getMaxX sset
   in length $ filter id (can'tContainBeacon sset . (`V2` 2000000) <$> enumFromTo min' max')

positionInBounds :: V2 Int -> Bool
positionInBounds (V2 x y)
  | x < 0 = False
  | x >= 4000000 = False
  | y < 0 = False
  | y >= 4000000 = False
  | otherwise = True

part2 :: [Measurement] -> Int
part2 input =
  let sset = fromMeasurements input
      positions = foldr (<>) Seq.empty $ fmap (\(x, y) -> positionsOfRadius x (y + 1)) (sset ^. sensors)
   in case find (\p -> not (can'tContainBeacon sset p) && positionInBounds p) positions of
        Nothing -> 0
        Just (V2 x y) -> x * 4000000 + y

run :: [String] -> IO ()
run [] = mapM_ (\x -> run [x]) ["1", "2"]
run ("1" : _) = runChallenge 2022 15 1 parseInput part1
run ("2" : _) = runChallenge 2022 15 2 parseInput part2
run (part : _) = error $ "Unknown part: " ++ part
