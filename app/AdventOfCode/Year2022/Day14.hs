module AdventOfCode.Year2022.Day14 where

import AdventOfCode.Fixture (runChallenge)
import Control.Monad.Trans.State (State, evalState, get, put)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Linear (V2)
import Linear.V2 (V2 (V2))
import Text.Parsec (char, sepBy1, sepEndBy1, string)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim (parse)
import Text.Parsec.String (Parser)
import Util (parseNumber)

type StateType = HashSet (V2 Int)

hitsBlock :: Int -> V2 Int -> StateType -> Bool
hitsBlock fy (V2 x y)
  | fy == y = const True
  | otherwise = HS.member (V2 x y)

step :: Int -> V2 Int -> State StateType (Either Bool (V2 Int))
step _ (V2 _ 200) = return $ Left True
step fy (V2 x y) = do
  hs <- get
  let cantGoDown = hitsBlock fy (V2 x (y + 1)) hs
  let cantGoDownLeft = hitsBlock fy (V2 (x - 1) (y + 1)) hs
  let cantGoDownRight = hitsBlock fy (V2 (x + 1) (y + 1)) hs

  case (cantGoDown, cantGoDownLeft, cantGoDownRight) of
    (False, _, _) -> return $ Right (V2 x (y + 1))
    (_, False, _) -> return $ Right (V2 (x - 1) (y + 1))
    (_, _, False) -> return $ Right (V2 (x + 1) (y + 1))
    _ -> do
      put $ HS.insert (V2 x y) hs
      return $ Left False

runSand :: Int -> V2 Int -> State StateType Bool
runSand fy pos = do
  res <- step fy pos
  case res of
    Left done -> return done
    Right pos' -> runSand fy pos'

runSimulation :: Int -> Int -> State StateType (Int, StateType)
runSimulation fy n = do
  res <- runSand fy (V2 500 0)
  hs <- get
  ( if res || hitsBlock fy (V2 500 0) hs
      then return (n, hs)
      else runSimulation fy (n + 1)
    )

plotHLine :: V2 Int -> Int -> StateType -> StateType
plotHLine _ 0 hs = hs
plotHLine (V2 x y) l hs = plotHLine (V2 (x + 1) y) (l - 1) (HS.insert (V2 x y) hs)

plotVLine :: V2 Int -> Int -> StateType -> StateType
plotVLine _ 0 hs = hs
plotVLine (V2 x y) l hs = plotVLine (V2 x (y + 1)) (l - 1) (HS.insert (V2 x y) hs)

setLine :: V2 Int -> V2 Int -> StateType -> StateType
setLine (V2 x1 y1) (V2 x2 y2)
  | x1 == x2 = if y1 <= y2 then plotVLine (V2 x1 y1) (y2 - y1 + 1) else setLine (V2 x2 y2) (V2 x1 y1)
  | y1 == y2 = if x1 <= x2 then plotHLine (V2 x1 y1) (x2 - x1 + 1) else setLine (V2 x2 y2) (V2 x1 y1)
  | otherwise = error "Only straight lines are supported"

setLines :: [V2 Int] -> StateType -> StateType
setLines [] hs = hs
setLines (x : xs) hs = snd $ foldl (\(src, hs') dst -> (dst, setLine src dst hs')) (x, hs) xs

positionParser :: Parser (V2 Int)
positionParser = do
  x <- parseNumber
  char ','
  V2 x <$> parseNumber

chainParser :: Parser [V2 Int]
chainParser = positionParser `sepBy1` string " -> "

inputParser :: Parser [[V2 Int]]
inputParser = chainParser `sepEndBy1` char '\n'

parseInput :: String -> Either ParseError [[V2 Int]]
parseInput = parse inputParser ""

getY :: V2 a -> a
getY (V2 _ y) = y

floorHeight :: [[V2 Int]] -> Int
floorHeight = (+ 2) . maximum . fmap (maximum . fmap getY)

part1 :: [[V2 Int]] -> Int
part1 = fst . evalState (runSimulation 201 0) . foldr setLines HS.empty

part2 :: [[V2 Int]] -> Int
part2 inp = 1 + fst (evalState (runSimulation (floorHeight inp) 0) $ foldr setLines HS.empty inp)

run :: [String] -> IO ()
run [] = mapM_ (\x -> run [x]) ["1", "2"]
run ("1" : _) = runChallenge 2022 14 1 parseInput part1
run ("2" : _) = runChallenge 2022 14 2 parseInput part2
run (part : _) = error $ "Unknown part: " ++ part
