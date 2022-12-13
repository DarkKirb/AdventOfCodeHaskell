module AdventOfCode.Year2022.Day13 where

import AdventOfCode.Fixture
import AdventOfCode.Year2022.Day11 (commaSeparatedList)
import Control.Applicative ((<|>))
import Data.Bifunctor (Bifunctor (first))
import Data.List (elemIndex, sort)
import Data.Maybe (fromJust)
import Text.Parsec (char, sepEndBy1)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim (parse)
import Text.Parsec.String (Parser)
import Util (enumerate, parseNumber, unzipMerge)

data Packet = Direct Int | Sublist [Packet]
  deriving (Show, Eq)

instance Ord Packet where
  compare :: Packet -> Packet -> Ordering
  compare (Direct a) (Direct b) = compare a b
  compare (Direct a) (Sublist bs) = compare (Sublist [Direct a]) (Sublist bs)
  compare (Sublist as) (Direct b) = compare (Sublist as) (Sublist [Direct b])
  compare (Sublist []) _ = LT
  compare _ (Sublist []) = GT
  compare (Sublist (a : as)) (Sublist (b : bs))
    | a == b = compare as bs
    | otherwise = compare a b

directParser :: Parser Packet
directParser = Direct <$> parseNumber

sublistParser :: Parser Packet
sublistParser = do
  char '['
  items <- commaSeparatedList packetParser
  char ']'
  return $ Sublist items

packetParser :: Parser Packet
packetParser = directParser <|> sublistParser

pairParser :: Parser (Packet, Packet)
pairParser = do
  p1 <- packetParser
  char '\n'
  p2 <- packetParser
  char '\n'
  return (p1, p2)

inputParser :: Parser [(Packet, Packet)]
inputParser = pairParser `sepEndBy1` char '\n'

parseInput :: String -> Either ParseError [(Packet, Packet)]
parseInput = parse inputParser ""

part1 :: [(Packet, Packet)] -> Int
part1 = sum . fmap fst . filter (uncurry (<) . snd) . fmap (first (+ 1)) . enumerate

part2 :: [(Packet, Packet)] -> Int
part2 i =
  let separators = (Sublist [Sublist [Direct 2]], Sublist [Sublist [Direct 6]])
      input = unzipMerge (separators : i)
      sorted = sort input
      fstIndex = fromJust $ elemIndex (fst separators) sorted
      sndIndex = fromJust $ elemIndex (snd separators) sorted
   in (fstIndex + 1) * (sndIndex + 1)

run :: [String] -> IO ()
run [] = mapM_ (\x -> run [x]) ["1", "2"]
run ("1" : _) = runChallenge 2022 13 1 parseInput part1
run ("2" : _) = runChallenge 2022 13 2 parseInput part2
run (part : _) = error $ "Unknown part: " ++ part
