module AdventOfCode.Year2015.Day6 where

import AdventOfCode.Fixture (runChallenge)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable (hashWithSalt))
import Text.Parsec (char, parse, sepEndBy1, string, try, (<|>))
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

toPositions :: Rectangle -> [[Position]]
toPositions r = fmap (\x -> fmap (\y -> Position (x, y)) [getY $ getMin r .. getY $ getMax r]) [getX $ getMin r .. getX $ getMax r]

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
  _ <- string "turn off "
  TurnOff <$> parseRectangle

parseTurnOn :: Parser Instruction
parseTurnOn = do
  _ <- string "turn on "
  TurnOn <$> parseRectangle

parseToggle :: Parser Instruction
parseToggle = do
  _ <- string "toggle "
  Toggle <$> parseRectangle

parseInstruction :: Parser Instruction
parseInstruction = try parseTurnOn <|> try parseTurnOff <|> parseToggle

parseLines :: Parser [Instruction]
parseLines = parseInstruction `sepEndBy1` char '\n'

parseInput :: String -> Either ParseError [Instruction]
parseInput = parse parseLines ""

turnOffLight :: Position -> HashSet Position -> HashSet Position
turnOffLight = HashSet.delete

turnOffLight' :: Position -> HashMap Position Int -> HashMap Position Int
turnOffLight' = HashMap.update (\i -> if i <= 1 then Nothing else Just (i - 1))

turnOnLight :: Position -> HashSet Position -> HashSet Position
turnOnLight = HashSet.insert

turnOnLight' :: Position -> HashMap Position Int -> HashMap Position Int
turnOnLight' = HashMap.alter (Just . maybe 1 (+ 1))

toggleLight :: Position -> HashSet Position -> HashSet Position
toggleLight p s = if HashSet.member p s then turnOffLight p s else turnOnLight p s

toggleLight' :: Position -> HashMap Position Int -> HashMap Position Int
toggleLight' = HashMap.alter (Just . maybe 2 (+ 2))

runInstruction :: Instruction -> HashSet Position -> HashSet Position
runInstruction (TurnOn r) s = foldr (flip $ foldr turnOnLight) s $ toPositions r
runInstruction (TurnOff r) s = foldr (flip $ foldr turnOffLight) s $ toPositions r
runInstruction (Toggle r) s = foldr (flip $ foldr toggleLight) s $ toPositions r

runInstruction' :: Instruction -> HashMap Position Int -> HashMap Position Int
runInstruction' (TurnOn r) s = foldr (flip $ foldr turnOnLight') s $ toPositions r
runInstruction' (TurnOff r) s = foldr (flip $ foldr turnOffLight') s $ toPositions r
runInstruction' (Toggle r) s = foldr (flip $ foldr toggleLight') s $ toPositions r

runInstructions :: [Instruction] -> HashSet Position -> HashSet Position
runInstructions = flip $ foldl (flip runInstruction)

runInstructions' :: [Instruction] -> HashMap Position Int -> HashMap Position Int
runInstructions' = flip $ foldl (flip runInstruction')

part1 :: [Instruction] -> Int
part1 input = HashSet.size $ runInstructions input HashSet.empty

part2 :: [Instruction] -> Int
part2 input = sum $ runInstructions' input HashMap.empty

run :: [String] -> IO ()
run [] = mapM_ (\x -> run [x]) ["1", "2"]
run ("1" : _) = runChallenge 2015 6 1 parseInput part1
run ("2" : _) = runChallenge 2015 6 2 parseInput part2
run (part : _) = error $ "Unknown part: " ++ part
