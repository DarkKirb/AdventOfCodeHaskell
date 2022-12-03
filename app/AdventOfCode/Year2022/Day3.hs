module AdventOfCode.Year2022.Day3 where

import AdventOfCode.Fixture (runChallenge)
import Data.Char (isAsciiLower, isAsciiUpper, ord)
import Util (findCommonElement, findCommonElementL, splitEvery, splitOnNewline)

parseLine :: String -> (String, String)
parseLine "" = ("", "")
parseLine s = case parseLine' 0 s of
  Right result -> result
  Left l -> error ("Did not find an answer (string = " ++ s ++ ", length = " ++ show l ++ ")")
  where
    parseLine' :: Int -> String -> Either Int (String, String)
    parseLine' n "" = Left n
    parseLine' nOld (x : xs) = case parseLine' (nOld + 1) xs of
      Left n -> if (nOld + 1) * 2 == n then Right ([x], xs) else Left n
      Right (first, second) -> Right (x : first, second)

parse :: String -> Either () [(String, String)]
parse = Right . fmap parseLine . splitOnNewline

parse2 :: String -> Either () [[String]]
parse2 = Right . splitEvery 3 . splitOnNewline

scoreLetter :: Char -> Int
scoreLetter c
  | isAsciiLower c = ord c - ord 'a' + 1
  | isAsciiUpper c = ord c - ord 'A' + 27
  | otherwise = 0

scoreForLine :: (String, String) -> Int
scoreForLine (a, b) = maybe 0 scoreLetter $ findCommonElement a b

scoreForGroup :: [String] -> Int
scoreForGroup = maybe 0 scoreLetter . findCommonElementL

part1 :: [(String, String)] -> Int
part1 = sum . fmap scoreForLine

part2 :: [[String]] -> Int
part2 = sum . fmap scoreForGroup

run :: [String] -> IO ()
run [] = mapM_ (\x -> run [x]) ["1", "2"]
run ("1" : _) = runChallenge 2022 3 1 parse part1
run ("2" : _) = runChallenge 2022 3 2 parse2 part2
run (part : _) = error $ "Unknown part: " ++ part
