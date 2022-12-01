module AdventOfCode.Year2015.Day5 where

import AdventOfCode.Fixture (runChallenge)
import Data.List (isInfixOf)
import Util (splitOnNewline)

isVowel :: Char -> Bool
isVowel 'a' = True
isVowel 'i' = True
isVowel 'u' = True
isVowel 'e' = True
isVowel 'o' = True
isVowel _ = False

containsThreeVowels :: Int -> String -> Bool
containsThreeVowels 3 _ = True
containsThreeVowels _ [] = False
containsThreeVowels n (x : xs)
  | isVowel x = containsThreeVowels (n + 1) xs
  | otherwise = containsThreeVowels n xs

repeatedLetters :: String -> Bool
repeatedLetters (x1 : x2 : xs)
  | x1 == x2 = True
  | otherwise = repeatedLetters (x2 : xs)
repeatedLetters _ = False

noNaughtyStrings :: String -> Bool
noNaughtyStrings ('a' : 'b' : _) = False
noNaughtyStrings ('c' : 'd' : _) = False
noNaughtyStrings ('p' : 'q' : _) = False
noNaughtyStrings ('x' : 'y' : _) = False
noNaughtyStrings (_ : xs) = noNaughtyStrings xs
noNaughtyStrings [] = True

repeatTwoLetters :: String -> Bool
repeatTwoLetters (x1 : x2 : xs)
  | [x1, x2] `isInfixOf` xs = True
  | otherwise = repeatTwoLetters (x2 : xs)
repeatTwoLetters _ = False

repeatLetterTwoApart :: String -> Bool
repeatLetterTwoApart (x1 : x2 : x3 : xs)
  | x1 == x3 = True
  | otherwise = repeatLetterTwoApart (x2 : x3 : xs)
repeatLetterTwoApart _ = False

isNice1 :: String -> Bool
isNice1 s = containsThreeVowels 0 s && repeatedLetters s && noNaughtyStrings s

isNice2 :: String -> Bool
isNice2 s = repeatTwoLetters s && repeatLetterTwoApart s

parse :: String -> Either () [String]
parse = Right . splitOnNewline

part1 :: [String] -> Int
part1 = length . filter isNice1

part2 :: [String] -> Int
part2 = length . filter isNice2

run :: [String] -> IO ()
run [] = mapM_ (\x -> run [x]) ["1", "2"]
run ("1" : _) = runChallenge 2015 5 1 parse part1
run ("2" : _) = runChallenge 2015 5 2 parse part2
run (part : _) = error $ "Unknown part: " ++ part
