module AdventOfCode.Year2022.Day12 where

import AdventOfCode.Fixture (runChallenge)
import Algorithm.Search (dijkstra)
import Data.Bifunctor (Bifunctor (first))
import Data.Char (isLower)
import Data.List (find)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Vector (Vector, (!?))
import qualified Data.Vector as V
import Linear (V2)
import Linear.V2 (V2 (..))
import Util (enumerate, splitOnNewline)

type HeightMap = Vector (Vector Int)

heightAt :: HeightMap -> V2 Int -> Maybe Int
heightAt hm pos = case hm !# pos of
  Just (-1) -> Just 0
  Just 26 -> Just 25
  z -> z

(!#) :: HeightMap -> V2 Int -> Maybe Int
hm !# (V2 x y) = hm !? x >>= (!? y)

isValidDestination :: Int -> Int -> Bool
isValidDestination from to = to - from <= 1

neighboringStates :: HeightMap -> V2 Int -> [V2 Int]
neighboringStates hm (V2 x y) =
  let height = fromMaybe 0 $ heightAt hm (V2 x y)
      potentialNeighbors =
        [ V2 x (y - 1),
          V2 x (y + 1),
          V2 (x - 1) y,
          V2 (x + 1) y
        ]
   in fmap fst $ filter (\(_, h) -> isValidDestination height h) $ mapMaybe (\p -> (p,) <$> heightAt hm p) potentialNeighbors

stateCost :: V2 Int -> V2 Int -> Int
stateCost _ _ = 1

stateFinish :: HeightMap -> V2 Int -> Bool
stateFinish hm pos = hm !# pos == Just 26

reverseTuple :: (a, b) -> (b, a)
reverseTuple (a, b) = (b, a)

findStartPosition :: HeightMap -> Maybe (V2 Int)
findStartPosition hm =
  find (\p -> hm !# p == Just (-1)) $ allStartPositions hm

allStartPositions :: HeightMap -> [V2 Int]
allStartPositions hm =
  let hmList = V.toList hm
      processRow :: Int -> Vector Int -> Maybe (V2 Int)
      processRow x row =
        let rowList = V.toList row
         in (fst <$> find (\(_, e) -> e <= 0) (first (V2 x) <$> enumerate rowList))
   in mapMaybe (uncurry processRow) (enumerate hmList)

parseHeight :: Char -> Int
parseHeight 'S' = -1
parseHeight 'E' = 26
parseHeight o
  | isLower o = fromEnum o - fromEnum 'a'
  | otherwise = error ("Invalid letter " ++ [o])

parseLine :: String -> Vector Int
parseLine = V.fromList . fmap parseHeight

parseInput :: String -> Either () HeightMap
parseInput = Right . V.fromList . fmap parseLine . splitOnNewline

part1 :: HeightMap -> Maybe Int
part1 hm = do
  startPos <- findStartPosition hm
  fst <$> dijkstra (neighboringStates hm) stateCost (stateFinish hm) startPos

part2 :: HeightMap -> Int
part2 hm =
  minimum
    $ mapMaybe
      ( fmap fst
          . dijkstra (neighboringStates hm) stateCost (stateFinish hm)
      )
    $ allStartPositions hm

run :: [String] -> IO ()
run [] = mapM_ (\x -> run [x]) ["1", "2"]
run ("1" : _) = runChallenge 2022 12 1 parseInput part1
run ("2" : _) = runChallenge 2022 12 2 parseInput part2
run (part : _) = error $ "Unknown part: " ++ part
