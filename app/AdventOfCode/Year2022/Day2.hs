module AdventOfCode.Year2022.Day2 where

import AdventOfCode.Fixture (runChallenge)
import Text.Parsec (char, parse)
import Text.Parsec.Combinator (sepEndBy1)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim ((<|>))
import Text.Parsec.String (Parser)

data Move = Rock | Paper | Scissors
  deriving (Show, Eq)

instance Ord Move where
  compare :: Move -> Move -> Ordering
  compare Rock Paper = LT
  compare Rock Scissors = GT
  compare Paper Rock = GT
  compare Paper Scissors = LT
  compare Scissors Rock = LT
  compare Scissors Paper = GT
  compare _ _ = EQ

parseOpponentMove :: Parser Move
parseOpponentMove =
  (char 'A' >> return Rock)
    <|> (char 'B' >> return Paper)
    <|> (char 'C' >> return Scissors)

parsePlayerMove :: Parser Move
parsePlayerMove =
  (char 'X' >> return Rock)
    <|> (char 'Y' >> return Paper)
    <|> (char 'Z' >> return Scissors)

parseMatch :: Parser (Move, Move)
parseMatch = do
  opp <- parseOpponentMove
  _ <- char ' '
  you <- parsePlayerMove
  return (opp, you)

parseMatches :: Parser [(Move, Move)]
parseMatches = parseMatch `sepEndBy1` char '\n'

parseInput :: String -> Either ParseError [(Move, Move)]
parseInput = parse parseMatches ""

scoreShape :: Move -> Int
scoreShape Rock = 1
scoreShape Paper = 2
scoreShape Scissors = 3

fixupMatch :: (Move, Move) -> (Move, Move)
fixupMatch (Rock, Rock) = (Rock, Scissors)
fixupMatch (Scissors, Rock) = (Scissors, Paper)
fixupMatch (x, Paper) = (x, x)
fixupMatch (Rock, Scissors) = (Rock, Paper)
fixupMatch (Scissors, Scissors) = (Scissors, Rock)
fixupMatch x = x

scoreMatch :: (Move, Move) -> Int
scoreMatch (opp, you) = matchResult + shapeResult
  where
    matchResult
      | opp == you = 3
      | opp > you = 0
      | otherwise = 6
    shapeResult = scoreShape you

scoreMatch' :: (Move, Move) -> Int
scoreMatch' = scoreMatch . fixupMatch

part1 :: [(Move, Move)] -> Int
part1 = sum . fmap scoreMatch

part2 :: [(Move, Move)] -> Int
part2 = sum . fmap scoreMatch'

run :: [String] -> IO ()
run [] = mapM_ (\x -> run [x]) ["1", "2"]
run ("1" : _) = runChallenge 2022 2 1 parseInput part1
run ("2" : _) = runChallenge 2022 2 2 parseInput part2
run (part : _) = error $ "Unknown part: " ++ part
