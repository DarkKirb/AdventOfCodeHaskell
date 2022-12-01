module AdventOfCode.Year2015.Day4 where

import AdventOfCode.Fixture (runChallenge)
import Crypto.Hash (Digest, MD5, hash)
import Data.ByteString.UTF8 as BSU (ByteString, fromString)
import Data.List (find)
import Util (enumerate)

md5 :: ByteString -> String
md5 = show . (hash :: ByteString -> Digest MD5)

leadingZeros :: String -> Int
leadingZeros ('0' : xs) = 1 + leadingZeros xs
leadingZeros _ = 0

genHash :: String -> Int -> String
genHash seed iter =
  let iterS = show iter
      input = seed ++ iterS
      inputB = BSU.fromString input
   in md5 inputB

genHashes :: String -> [String]
genHashes seed = fmap (genHash (init seed)) [0 ..]

part1 :: String -> Maybe Int
part1 = fmap fst . find (\(_, c) -> leadingZeros c >= 5) . enumerate . genHashes

part2 :: String -> Maybe Int
part2 = fmap fst . find (\(_, c) -> leadingZeros c >= 6) . enumerate . genHashes

run :: IO ()
run = do
  runChallenge 2015 4 1 part1
  runChallenge 2015 4 2 part2
