module Util where

import Data.List (sort)
import Text.Parsec (digit, many1, option)
import Text.Parsec.Char (char)
import Text.Parsec.Prim ((<?>))
import Text.Parsec.String (Parser)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

runningSum :: Num a => [a] -> [a]
runningSum = scanl (+) 0

parseNumber :: Parser Int
parseNumber = read <$> many1 digit <?> "number"

parseSNumber :: Parser Int
parseSNumber = do
  sign <- option 1 (char '-' >> return (-1))
  number <- parseNumber
  return $ sign * number

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

applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ s = s
applyN n f s = applyN (n - 1) f $ f s

applyNM :: Monad m => Int -> m () -> m ()
applyNM 0 m = m
applyNM n m = m >> applyNM (n - 1) m

applyN' :: Int -> (a -> a) -> a -> [a]
applyN' 0 _ s = [s]
applyN' n f s =
  let next = f s
   in next : applyN' (n - 1) f next

allBinaryPairs :: [a] -> [(a, a)]
allBinaryPairs (x : xs) = fmap (x,) xs ++ allBinaryPairs xs
allBinaryPairs [] = []

permReduce :: [a] -> (a -> a -> b) -> (b -> b -> b) -> b -> b
permReduce xs f1 f2 f2nil = foldr f2 f2nil (fmap (uncurry f1) (allBinaryPairs xs))

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow n xs
  | length xs < n = []
  | otherwise =
      let (front, _) = splitAt n xs
       in front : slidingWindow n (tail xs)

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_ : xs) = Just xs

(<.>) :: Functor f => (a -> b) -> (c -> a) -> f c -> f b
f <.> g = fmap (f . g)

unzipMerge :: [(a, a)] -> [a]
unzipMerge [] = []
unzipMerge ((a, b) : xs) = a : b : unzipMerge xs
