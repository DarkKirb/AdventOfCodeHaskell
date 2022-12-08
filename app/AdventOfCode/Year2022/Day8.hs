module AdventOfCode.Year2022.Day8 where

import AdventOfCode.Fixture (runChallenge)
import Data.Char (digitToInt, isDigit)
import Data.List (sort, transpose)
import Util (enumerate, requireAllRight, safeTail, splitOnNewline, uniq)

parseDigit :: Char -> Either () Int
parseDigit c
  | isDigit c = Right $ digitToInt c
  | otherwise = Left ()

parseInput :: String -> Either () [[Int]]
parseInput input = init <$> requireAllRight (requireAllRight . fmap parseDigit <$> splitOnNewline input)

visibleInRow :: [Int] -> [Int]
visibleInRow = visibleInRow' (-1) 0
  where
    visibleInRow' _ _ [] = []
    visibleInRow' n pos (x : xs)
      | n < x = pos : visibleInRow' x (pos + 1) xs
      | otherwise = visibleInRow' n (pos + 1) xs

visibleInRowReverse :: [Int] -> [Int]
visibleInRowReverse r = fmap (l -) $ visibleInRow $ reverse r
  where
    l = length r - 1

visibleInRowBidi :: [Int] -> [Int]
visibleInRowBidi r = visibleInRow r ++ visibleInRowReverse r

visibleInRows :: [[Int]] -> [(Int, Int)]
visibleInRows = concatMap (\(i, r) -> (i,) <$> visibleInRowBidi r) . enumerate

visibleInColumns :: [[Int]] -> [(Int, Int)]
visibleInColumns = fmap (\(a, b) -> (b, a)) . visibleInRows . transpose

visibleTrees :: [[Int]] -> [(Int, Int)]
visibleTrees input = uniq $ sort (visibleInRows input ++ visibleInColumns input)

lineOfSight :: Int -> [Int] -> Int
lineOfSight _ [] = 0
lineOfSight n (x : xs)
  | n <= x = 1
  | otherwise = 1 + lineOfSight n xs

visibleTreesFromSpot :: [[Int]] -> (Int, Int) -> Int
visibleTreesFromSpot forest (x, y) =
  let row = forest !! y
      height = row !! x
      (left, right) = splitAt x row
      column = transpose forest !! x
      (up, down) = splitAt y column
      visibleLeft = lineOfSight height $ reverse left
      visibleRight = maybe 0 (lineOfSight height) (safeTail right)
      visibleUp = lineOfSight height $ reverse up
      visibleDown = maybe 0 (lineOfSight height) (safeTail down)
   in visibleLeft * visibleRight * visibleUp * visibleDown

part1 :: [[Int]] -> Int
part1 = length . visibleTrees

part2 :: [[Int]] -> Int
part2 input =
  let width = length input
   in maximum (visibleTreesFromSpot input <$> concatMap (\x -> fmap (x,) [0 .. width - 1]) [0 .. width - 1])

run :: [String] -> IO ()
run [] = mapM_ (\x -> run [x]) ["1", "2"]
run ("1" : _) = runChallenge 2022 8 1 parseInput part1
run ("2" : _) = runChallenge 2022 8 2 parseInput part2
run (part : _) = error $ "Unknown part: " ++ part
