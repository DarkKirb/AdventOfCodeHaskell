module AdventOfCode.Year2015.Day6 where

import AdventOfCode.Fixture (runChallenge)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable (hashWithSalt))
import Text.Parsec (char, parse, string, (<|>))
import Text.Parsec.Combinator (many1)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim ((<?>))
import Text.Parsec.String (Parser)
import Util (parseNumber)

newtype Position = Position (Int, Int)
  deriving (Eq, Show, Ord)

instance Hashable Position where
  hashWithSalt :: Int -> Position -> Int
  hashWithSalt salt v = hashWithSalt salt (getX v, getY v)

getX :: Position -> Int
getX (Position (x, _)) = x

getY :: Position -> Int
getY (Position (_, y)) = y

newtype Rectangle = Rectangle (Position, Position)
  deriving (Eq, Show)

getMin :: Rectangle -> Position
getMin (Rectangle (m, _)) = m

getMax :: Rectangle -> Position
getMax (Rectangle (_, m)) = m

toPositions :: Rectangle -> [Position]
toPositions r = concatMap (\x -> fmap (\y -> Position (x, y)) [getY $ getMin r .. getY $ getMax r]) [getX $ getMin r .. getX $ getMax r]

data Instruction = TurnOn Rectangle | TurnOff Rectangle | Toggle Rectangle
  deriving (Eq, Show)

parsePosition :: Parser Position
parsePosition =
  do
    x <- parseNumber
    _ <- char ','
    y <- parseNumber
    return (Position (x, y))
    <?> "Position"

parseRectangle :: Parser Rectangle
parseRectangle =
  do
    from <- parsePosition
    _ <- string " through "
    to <- parsePosition
    return (Rectangle (from, to))
    <?> "Rectangle"

parseTurnOff :: Parser Instruction
parseTurnOff = do
  _ <- string "ff "
  TurnOff <$> parseRectangle

parseTurnOn :: Parser Instruction
parseTurnOn = do
  _ <- string "n "
  TurnOn <$> parseRectangle

parseTurnO :: Parser Instruction
parseTurnO = do
  _ <- string "urn o"
  parseTurnOff <|> parseTurnOn

parseToggle :: Parser Instruction
parseToggle = do
  _ <- string "oggle "
  Toggle <$> parseRectangle

parseInstruction :: Parser Instruction
parseInstruction = do
  _ <- char 't'
  parseTurnO <|> parseToggle <?> "Instruction"

parseLine :: Parser Instruction
parseLine = parseInstruction >>= \ins -> char '\n' >> return ins

parseLines :: Parser [Instruction]
parseLines = many1 parseLine

parseInput :: String -> Either ParseError [Instruction]
parseInput = parse parseLines ""

turnOffLight :: Position -> HashSet Position -> HashSet Position
turnOffLight = HashSet.delete

turnOnLight :: Position -> HashSet Position -> HashSet Position
turnOnLight = HashSet.insert

toggleLight :: Position -> HashSet Position -> HashSet Position
toggleLight p s = if HashSet.member p s then turnOffLight p s else turnOnLight p s

runInstruction :: Instruction -> HashSet Position -> HashSet Position
runInstruction (TurnOn r) s = foldr turnOnLight s (toPositions r)
runInstruction (TurnOff r) s = foldr turnOffLight s (toPositions r)
runInstruction (Toggle r) s = foldr toggleLight s (toPositions r)

runInstructions :: [Instruction] -> HashSet Position -> HashSet Position
runInstructions = flip $ foldr runInstruction

part1 :: String -> Either ParseError Int
part1 input = do
  input' <- parseInput input
  Right (HashSet.size $ runInstructions input' HashSet.empty)

run :: IO ()
run = do
  runChallenge 2015 6 1 part1
