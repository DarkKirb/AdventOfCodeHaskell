module AdventOfCode.Year2015.Day3 where

import AdventOfCode.Fixture (runChallenge)
import Control.Monad.Trans.State (State, evalState, get, put)
import Data.Set (Set)
import qualified Data.Set as Set

type ResultType = Int

type StateType1 = ((Int, Int), Set (Int, Int))

type StateType2 = (Bool, (Int, Int), (Int, Int), Set (Int, Int))

part1M :: String -> State StateType1 ResultType
part1M [] = do
  (_, set) <- get
  return $ Set.size set
part1M (c : xs) = do
  ((x, y), set) <- get
  case c of
    '^' -> put ((x, y + 1), Set.insert (x, y + 1) set)
    'v' -> put ((x, y - 1), Set.insert (x, y - 1) set)
    '<' -> put ((x - 1, y), Set.insert (x - 1, y) set)
    '>' -> put ((x + 1, y), Set.insert (x + 1, y) set)
    _ -> put ((x, y), set)
  part1M xs

part2M :: String -> State StateType2 ResultType
part2M [] = do
  (_, _, _, set) <- get
  return $ Set.size set
part2M (c : xs) = do
  (round', pos1, pos2, set) <- get
  let (x, y) = if round' then pos2 else pos1
  case c of
    '^' | round' -> put (False, pos1, (x, y + 1), Set.insert (x, y + 1) set)
    '^' | not round' -> put (True, (x, y + 1), pos2, Set.insert (x, y + 1) set)
    'v' | round' -> put (False, pos1, (x, y - 1), Set.insert (x, y - 1) set)
    'v' | not round' -> put (True, (x, y - 1), pos2, Set.insert (x, y - 1) set)
    '<' | round' -> put (False, pos1, (x - 1, y), Set.insert (x - 1, y) set)
    '<' | not round' -> put (True, (x - 1, y), pos2, Set.insert (x - 1, y) set)
    '>' | round' -> put (False, pos1, (x + 1, y), Set.insert (x + 1, y) set)
    '>' | not round' -> put (True, (x + 1, y), pos2, Set.insert (x + 1, y) set)
    _ -> put (round', pos1, pos2, set)
  part2M xs

startState1 :: StateType1
startState1 = ((0, 0), Set.singleton (0, 0))

startState2 :: StateType2
startState2 = (False, (0, 0), (0, 0), Set.singleton (0, 0))

part1 :: String -> Int
part1 input = evalState (part1M input) startState1

part2 :: String -> Int
part2 input = evalState (part2M input) startState2

run :: IO ()
run = do
  runChallenge 2015 3 1 part1
  runChallenge 2015 3 2 part2
