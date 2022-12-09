module AdventOfCode.Year2022.Day9 where

import AdventOfCode.Fixture (runChallenge)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Linear (V2)
import Linear.V2 (V2 (..))
import Text.Parsec (parse, string)
import Text.Parsec.Char (char)
import Text.Parsec.Combinator (sepEndBy1)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim ((<|>))
import Text.Parsec.String (Parser)
import Util (applyN, parseNumber)

newtype Move = Move (V2 Int)

leftParser :: Parser Move
leftParser = do
  string "L "
  amount <- parseNumber
  return $ Move $ V2 (-amount) 0

rightParser :: Parser Move
rightParser = do
  string "R "
  amount <- parseNumber
  return $ Move $ V2 amount 0

upParser :: Parser Move
upParser = do
  string "U "
  Move . V2 0 <$> parseNumber

downParser :: Parser Move
downParser = do
  string "D "
  amount <- parseNumber
  return $ Move $ V2 0 (-amount)

moveParser :: Parser Move
moveParser = leftParser <|> rightParser <|> upParser <|> downParser

inputParser :: Parser [Move]
inputParser = moveParser `sepEndBy1` char '\n'

parseInput :: String -> Either ParseError [Move]
parseInput = parse inputParser ""

updateNodePos :: V2 Int -> V2 Int -> V2 Int
updateNodePos h t = case t - h of
  V2 2 0 -> t - V2 1 0
  V2 2 1 -> t - V2 1 1
  V2 2 2 -> t - V2 1 1
  V2 1 2 -> t - V2 1 1
  V2 0 2 -> t - V2 0 1
  V2 (-1) 2 -> t - V2 (-1) 1
  V2 (-2) 2 -> t - V2 (-1) 1
  V2 (-2) 1 -> t - V2 (-1) 1
  V2 (-2) 0 -> t - V2 (-1) 0
  V2 (-2) (-1) -> t - V2 (-1) (-1)
  V2 (-2) (-2) -> t - V2 (-1) (-1)
  V2 (-1) (-2) -> t - V2 (-1) (-1)
  V2 0 (-2) -> t - V2 0 (-1)
  V2 1 (-2) -> t - V2 1 (-1)
  V2 2 (-2) -> t - V2 1 (-1)
  V2 2 (-1) -> t - V2 1 (-1)
  _ -> t

updateRope :: [V2 Int] -> [V2 Int]
updateRope [] = []
updateRope [x] = [x]
updateRope (x : xs) =
  let updatedRest = updateRope xs
      updatedHead = updateNodePos (head updatedRest) x
   in updatedHead : updatedRest

type State = (HashSet (V2 Int), [V2 Int])

stepOnce :: State -> Move -> State
stepOnce (hs, rope) (Move d) =
  let newHead = last rope + d
      tmpRope = init rope ++ [newHead]
      newRope = updateRope tmpRope
   in (HS.insert (head newRope) hs, newRope)

step :: State -> Move -> State
step state (Move (V2 x 0)) = applyN (abs x) (`stepOnce` Move (V2 (signum x) 0)) state
step state (Move (V2 0 y)) = applyN (abs y) (`stepOnce` Move (V2 0 (signum y))) state
step state (Move (V2 x y)) = step (step state (Move (V2 x 0))) (Move (V2 0 y))

commonPart :: Int -> [Move] -> Int
commonPart n = length . fst . foldl step (HS.empty, replicate n $ V2 0 0)

part1 :: [Move] -> Int
part1 = commonPart 2

part2 :: [Move] -> Int
part2 = commonPart 10

run :: [String] -> IO ()
run [] = mapM_ (\x -> run [x]) ["1", "2"]
run ("1" : _) = runChallenge 2022 9 1 parseInput part1
run ("2" : _) = runChallenge 2022 9 2 parseInput part2
run (part : _) = error $ "Unknown part: " ++ part
