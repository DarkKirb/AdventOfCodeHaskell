module AdventOfCode.Year2022.Day6 where

import AdventOfCode.Fixture (runChallenge, runTest)
import Control.Monad (forM_)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Util (enumerate, permReduce, slidingWindow)

neqAll :: Eq a => [a] -> Bool
neqAll xs = permReduce xs (/=) (&&) True

parse :: String -> Either () String
parse = Right

parts :: Int -> String -> Int
parts n = (+) n . fst . fromMaybe (error "Not found") . find (neqAll . snd) . enumerate . slidingWindow n

part1 :: String -> Int
part1 = parts 4

part2 :: String -> Int
part2 = parts 14

tests1 :: [(String, Int)]
tests1 =
  [ ("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 7),
    ("bvwbjplbgvbhsrlpgdmjqwftvncz", 5),
    ("nppdvjthqldpwncqszvftbrmjlhg", 6),
    ("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 10),
    ("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 11)
  ]

tests2 :: [(String, Int)]
tests2 =
  [ ("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 19),
    ("bvwbjplbgvbhsrlpgdmjqwftvncz", 23),
    ("nppdvjthqldpwncqszvftbrmjlhg", 23),
    ("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 29),
    ("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 26)
  ]

run :: [String] -> IO ()
run [] = mapM_ (\x -> run [x]) ["1", "2"]
run ("1" : _) = runChallenge 2022 6 1 parse part1
run ("test1" : _) = forM_ tests1 (uncurry $ runTest parse part1)
run ("2" : _) = runChallenge 2022 6 2 parse part2
run ("test2" : _) = forM_ tests2 (uncurry $ runTest parse part2)
run (part : _) = error $ "Unknown part: " ++ part
