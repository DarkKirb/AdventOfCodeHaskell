module AdventOfCode.Year2022.Day10 where

import AdventOfCode.Fixture (runChallenge)
import Text.Parsec (parse, sepEndBy1, string)
import Text.Parsec.Char (char)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim ((<|>))
import Text.Parsec.String (Parser)
import Util (applyN', parseSNumber)

data Instruction = Noop | Addition Int
  deriving (Show)

noopParser :: Parser Instruction
noopParser = string "noop" >> return Noop

addxParser :: Parser Instruction
addxParser = string "addx " >> Addition <$> parseSNumber

instructionParser :: Parser Instruction
instructionParser = noopParser <|> addxParser

inputParser :: Parser [Instruction]
inputParser = instructionParser `sepEndBy1` char '\n'

parseInput :: String -> Either ParseError [Instruction]
parseInput = parse inputParser ""

type State = (Int, Int, [Instruction])

step :: State -> State
step (x, cStep, []) = (x, cStep, [])
step (x, 0, Noop : xs) = (x, 0, xs)
step (x, 0, Addition i : xs) = (x, 1, Addition i : xs)
step (x, 1, Addition i : xs) = (x + i, 0, xs)
step _ = error "Unknown state"

isPixelLit :: Int -> Int -> Bool
isPixelLit col spritePos = case spritePos - col of
  -1 -> True
  0 -> True
  1 -> True
  _ -> False

stepCRT :: Int -> State -> (Char, Int)
stepCRT col (spritePos, _, _) = (if isPixelLit col spritePos then '#' else '.', if col == 39 then 0 else col + 1)

simulate :: [Instruction] -> [Int]
simulate = fmap (\(x, _, _) -> x) . applyN' 220 step . (1,0,)

simulateCRT :: [Instruction] -> String
simulateCRT ins =
  let steps = (1, 0, ins) : applyN' 240 step (1, 0, ins)
      simulateSteps' :: Int -> [State] -> String
      simulateSteps' _ [] = ""
      simulateSteps' n (s : ss) =
        let (c, next) = stepCRT n s
            rest = simulateSteps' next ss
         in if next == 0 then c : '\n' : rest else c : rest
   in simulateSteps' 0 steps

signalStrength :: [Int] -> Int -> Int
signalStrength xs n = (xs !! (n - 2)) * n

part1 :: [Instruction] -> Int
part1 ins =
  let simulation = simulate ins
   in sum $ fmap (signalStrength simulation) [20, 60, 100, 140, 180, 220]

part2 :: [Instruction] -> String
part2 = simulateCRT

run :: [String] -> IO ()
run [] = mapM_ (\x -> run [x]) ["1", "2"]
run ("1" : _) = runChallenge 2022 10 1 parseInput part1
run ("2" : _) = runChallenge 2022 10 2 parseInput part2
run (part : _) = error $ "Unknown part: " ++ part
