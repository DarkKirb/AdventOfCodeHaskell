module AdventOfCode.Year2022.Day4 where

import AdventOfCode.Fixture (runChallenge)
import Text.Parsec (char, parse)
import Text.Parsec.Combinator (sepEndBy1)
import Text.Parsec.Error (ParseError)
import Text.Parsec.String (Parser)
import Util (parseNumber)

data Range = Range Int Int

(..=) :: Int -> Int -> Range
a ..= b = Range a b

getMinimum :: Range -> Int
getMinimum (Range x _) = x

getMaximum :: Range -> Int
getMaximum (Range _ x) = x

inRange :: Int -> Range -> Bool
inRange n r = n >= getMinimum r && n <= getMaximum r

(⊆) :: Range -> Range -> Bool
r1 ⊆ r2 = inRange (getMinimum r1) r2 && inRange (getMaximum r1) r2

(⊇) :: Range -> Range -> Bool
r2 ⊇ r1 = r1 ⊆ r2

(⊆∨⊇) :: Range -> Range -> Bool
r1 ⊆∨⊇ r2 = r1 ⊆ r2 || r1 ⊇ r2

isOverlapping :: Range -> Range -> Bool
isOverlapping (Range x1 y1) (Range x2 y2) = (x1 `max` x2) <= (y1 `min` y2)

parseRange :: Parser Range
parseRange = do
  min' <- parseNumber
  char '-'
  max' <- parseNumber
  return (min' ..= max')

parsePair :: Parser (Range, Range)
parsePair = do
  range1 <- parseRange
  char ','
  range2 <- parseRange
  return (range1, range2)

inputParser :: Parser [(Range, Range)]
inputParser = parsePair `sepEndBy1` char '\n'

parseInput :: String -> Either ParseError [(Range, Range)]
parseInput = parse inputParser ""

part1 :: [(Range, Range)] -> Int
part1 = length . filter (uncurry (⊆∨⊇))

part2 :: [(Range, Range)] -> Int
part2 = length . filter (uncurry isOverlapping)

run :: [String] -> IO ()
run [] = mapM_ (\x -> run [x]) ["1", "2"]
run ("1" : _) = runChallenge 2022 4 1 parseInput part1
run ("2" : _) = runChallenge 2022 4 2 parseInput part2
run (part : _) = error $ "Unknown part: " ++ part
