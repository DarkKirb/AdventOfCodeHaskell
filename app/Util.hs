module Util where

import Text.Parsec (digit, many1)
import Text.Parsec.Prim ((<?>))
import Text.Parsec.String (Parser)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

runningSum :: Num a => [a] -> [a]
runningSum = scanl (+) 0

parseNumber :: Parser Int
parseNumber = read <$> many1 digit <?> "number"

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen _ [] = [[]]
splitWhen f (x : xs) =
  let rest = splitWhen f xs
   in if f x
        then [] : rest
        else (x : head rest) : tail rest

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn a = splitWhen (== a)

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
