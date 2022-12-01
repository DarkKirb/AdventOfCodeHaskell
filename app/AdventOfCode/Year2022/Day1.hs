module AdventOfCode.Year2022.Day1 where

import AdventOfCode.Fixture (runChallenge)
import Data.List (sortBy)
import Text.Parsec (char, sepEndBy)
import Text.Parsec.Combinator (sepBy1)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim (parse)
import Text.Parsec.String (Parser)
import Util (parseNumber)

inputParser :: Parser [[Int]]
inputParser = (parseNumber `sepEndBy` char '\n') `sepBy1` char '\n'

parseInput :: String -> Either ParseError [[Int]]
parseInput = parse inputParser ""

part1 :: String -> Either ParseError Int
part1 input = do
  input' <- parseInput input
  Right $ maximum $ fmap sum input'

part2 :: String -> Either ParseError Int
part2 input = do
  input' <- parseInput input
  Right $ sum $ take 3 $ sortBy (flip compare) $ fmap sum input'

run :: IO ()
run = do
  runChallenge 2022 1 1 part1
  runChallenge 2022 1 2 part2
