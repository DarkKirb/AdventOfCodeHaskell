module AdventOfCode.Year2015.Day2 where

import AdventOfCode.Fixture (runChallenge)
import Text.Parsec (ParseError, char, many1, parse)
import Text.Parsec.Prim ((<?>))
import Text.Parsec.String (Parser)
import Util (parseNumber, uncurry3)

calcSurfaceArea :: Int -> Int -> Int -> Int
calcSurfaceArea l w h = 2 * lw + 2 * wh + 2 * hl + (lw `min` wh `min` hl)
  where
    lw = l * w
    wh = w * h
    hl = h * l

min2 :: Int -> Int -> Int -> (Int, Int)
min2 a b c
  | absoluteMaximum == a = (b, c)
  | absoluteMaximum == b = (a, c)
  | otherwise = (a, b)
  where
    absoluteMaximum = a `max` b `max` c

calcRibbonLength :: Int -> Int -> Int -> Int
calcRibbonLength l w h = 2 * a + 2 * b + l * w * h
  where
    (a, b) = min2 l w h

parseLine :: Parser (Int, Int, Int)
parseLine =
  do
    dim1 <- parseNumber
    _ <- char 'x'
    dim2 <- parseNumber
    _ <- char 'x'
    dim3 <- parseNumber
    _ <- char '\n'
    return (dim1, dim2, dim3)
    <?> "input line"

inputParser :: Parser [(Int, Int, Int)]
inputParser = many1 parseLine

parseInput :: String -> Either ParseError [(Int, Int, Int)]
parseInput = parse inputParser ""

part1 :: String -> Either ParseError Int
part1 input = do
  input' <- parseInput input
  Right ((sum . fmap (uncurry3 calcSurfaceArea)) input')

part2 :: String -> Either ParseError Int
part2 input = do
  input' <- parseInput input
  Right ((sum . fmap (uncurry3 calcRibbonLength)) input')

run :: IO ()
run = do
  runChallenge 2015 2 1 part1
  runChallenge 2015 2 2 part2
