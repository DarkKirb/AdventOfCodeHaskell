module AdventOfCode.Year2022.Day5 where

import AdventOfCode.Fixture (runChallenge)
import Control.Monad (forM_)
import Data.List (transpose)
import Data.Maybe (catMaybes)
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import Text.Parsec (many1, parse, sepEndBy1, string, try, upper)
import Text.Parsec.Char (char)
import Text.Parsec.Combinator (sepBy1)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim ((<|>))
import Text.Parsec.String (Parser)
import Util (applyN, parseNumber)

type State = Vector [Char]

presentBoxParser :: Parser Char
presentBoxParser = do
  char '['
  letter <- upper
  char ']'
  return letter

boxParser :: Parser (Maybe Char)
boxParser = try (string "   " >> return Nothing) <|> Just <$> presentBoxParser

initStateLineParser :: Parser [Maybe Char]
initStateLineParser = boxParser `sepBy1` char ' '

initStateHead :: Parser State
initStateHead = correctState <$> initStateLineParser `sepEndBy1` char '\n'

correctState :: [[Maybe Char]] -> State
correctState = V.fromList . fmap catMaybes . transpose

initState :: Parser State
initState = do
  res <- initStateHead
  forM_
    [1 .. length res]
    ( \i -> do
        many1 $ char ' '
        string $ show i
        return Nothing
    )
  char '\n'
  char '\n'
  return res

data Command = Command Int Int Int
  deriving (Show)

commandParser :: Parser Command
commandParser = do
  string "move "
  count <- parseNumber
  string " from "
  source <- parseNumber
  string " to "
  dest <- parseNumber
  return $ Command count (source - 1) (dest - 1)

commandsParser :: Parser [Command]
commandsParser = commandParser `sepEndBy1` char '\n'

inputParser :: Parser (State, [Command])
inputParser = do
  state <- initState
  commands <- commandsParser
  return (state, commands)

parseInput :: String -> Either ParseError (State, [Command])
parseInput = parse inputParser ""

moveBox :: Int -> Int -> State -> State
moveBox from to s =
  let (item : rest) = s ! from
      newDest = item : (s ! to)
      updates = [(from, rest), (to, newDest)]
   in s // updates

applyCommand :: State -> Command -> State
applyCommand s (Command number from to) = applyN number (moveBox from to) s

applyCommand' :: State -> Command -> State
applyCommand' s (Command number from to) =
  let fromStack = s ! from
      (start, rest) = splitAt number fromStack
      newDest = start ++ (s ! to)
      updates = [(from, rest), (to, newDest)]
   in s // updates

applyCommands :: State -> [Command] -> State
applyCommands = foldl applyCommand

applyCommands' :: State -> [Command] -> State
applyCommands' = foldl applyCommand'

part1 :: (State, [Command]) -> String
part1 (state, commands) = fmap head $ V.toList $ applyCommands state commands

part2 :: (State, [Command]) -> String
part2 (state, commands) = fmap head $ V.toList $ applyCommands' state commands

run :: [String] -> IO ()
run [] = mapM_ (\x -> run [x]) ["1", "2"]
run ("1" : _) = runChallenge 2022 5 1 parseInput part1
run ("2" : _) = runChallenge 2022 5 2 parseInput part2
run (part : _) = error $ "Unknown part: " ++ part
