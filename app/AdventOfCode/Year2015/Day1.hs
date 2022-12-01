module AdventOfCode.Year2015.Day1 where

import AdventOfCode.Fixture (runChallenge)
import Util (enumerate, runningSum)

parseChar :: Char -> Int
parseChar '(' = 1
parseChar ')' = -1
parseChar _ = 0

part1 :: String -> Int
part1 = sum . fmap parseChar

part2 :: String -> Int
part2 = head . fmap fst . filter (\x -> snd x < 0) . enumerate . runningSum . fmap parseChar

run :: IO ()
run = do
  runChallenge 2015 1 1 part1
  runChallenge 2015 1 2 part2
