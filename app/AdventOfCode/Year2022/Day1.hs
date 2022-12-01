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

part1 :: [[Int]] -> Int
part1 = maximum . fmap sum

part2 :: [[Int]] -> Int
part2 = sum . take 3 . sortBy (flip compare) . fmap sum

run :: [String] -> IO ()
run [] = mapM_ (\x -> run [x]) ["1", "2"]
run ("1" : _) = runChallenge 2022 1 1 parseInput part1
run ("2" : _) = runChallenge 2022 1 2 parseInput part2
run (part : _) = error $ "Unknown part: " ++ part
