module Util where

import Data.List (sort)
import Text.Parsec (digit, many1)
import Text.Parsec.Prim ((<?>))
import Text.Parsec.String (Parser)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

runningSum :: Num a => [a] -> [a]
runningSum = scanl (+) 0

parseNumber :: Parser Int
parseNumber = read <$> many1 digit <?> "number"

splitWhenI :: (Int -> a -> Bool) -> [a] -> [[a]]
splitWhenI = splitWhenI' 0
  where
    splitWhenI' _ _ [] = [[]]
    splitWhenI' i f (x : xs) =
      let rest = splitWhenI' (i + 1) f xs
       in if f i x
            then [] : rest
            else (x : head rest) : tail rest

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen f = splitWhenI $ const f

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn a = splitWhen (== a)

splitEvery :: Int -> [a] -> [[a]]
splitEvery = splitEvery' 1
  where
    splitEvery' _ _ [] = [[]]
    splitEvery' a b (x : xs)
      | a == b = [x] : splitEvery' 1 b xs
      | otherwise = (x : head rest) : tail rest
      where
        rest = splitEvery' (a + 1) b xs

splitOnNewline :: String -> [String]
splitOnNewline = splitOn '\n'

requireAllJust :: [Maybe a] -> Maybe [a]
requireAllJust [] = Just []
requireAllJust (Nothing : _) = Nothing
requireAllJust (Just a : xs) = requireAllJust xs >>= \xs' -> Just (a : xs')

requireAllRight :: [Either a b] -> Either a [b]
requireAllRight [] = Right []
requireAllRight (Left a : _) = Left a
requireAllRight (Right b : xs) = requireAllRight xs >>= \xs' -> Right (b : xs')

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f x = f a b c where (a, b, c) = x

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

uniq :: Eq a => [a] -> [a]
uniq (x1 : x2 : xs)
  | x1 == x2 = uniq (x1 : xs)
  | otherwise = x1 : uniq (x2 : xs)
uniq xs = xs

findCommonElements :: Ord a => [a] -> [a] -> [a]
findCommonElements a b = uniq $ findCommonElements' (sort a) (sort b)
  where
    findCommonElements' [] _ = []
    findCommonElements' _ [] = []
    findCommonElements' (x1 : xs1) (x2 : xs2)
      | x1 == x2 = x1 : findCommonElements' xs1 xs2
      | x1 < x2 = findCommonElements' xs1 (x2 : xs2)
      | otherwise = findCommonElements' (x1 : xs1) xs2

findCommonElementsL :: Ord a => [[a]] -> [a]
findCommonElementsL [x] = x
findCommonElementsL (x : xs) = findCommonElements x $ findCommonElementsL xs
findCommonElementsL [] = []

findCommonElement :: Ord a => [a] -> [a] -> Maybe a
findCommonElement a b = safeHead $ findCommonElements a b

findCommonElementL :: Ord a => [[a]] -> Maybe a
findCommonElementL = safeHead . findCommonElementsL
