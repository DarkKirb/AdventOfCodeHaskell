module AdventOfCode.Year2015.Day1 where

import AdventOfCode.Fixture (runChallenge)
import Util (enumerate, runningSum)

parseChar :: Char -> Int
parseChar '(' = 1
parseChar ')' = -1
parseChar _ = 0

part1 :: [Int] -> Int
part1 = sum

part2 :: [Int] -> Int
part2 = head . fmap fst . filter (\x -> snd x < 0) . enumerate . runningSum

parse :: String -> Either () [Int]
parse = Right . fmap parseChar

run :: [String] -> IO ()
run [] = mapM_ (\x -> run [x]) ["1", "2"]
run ("1" : _) = runChallenge 2015 1 1 parse part1
run ("2" : _) = runChallenge 2015 1 2 parse part2
run (part : _) = error $ "Unknown part: " ++ part
