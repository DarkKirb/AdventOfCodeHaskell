module Main where

import qualified AdventOfCode.Year2015 as Year2015
import qualified AdventOfCode.Year2022 as Year2022
import System.Environment (getArgs)

run :: [String] -> IO ()
run [] =
  mapM_
    (\x -> run [x])
    [ "2015",
      "2022"
    ]
run ("2015" : xs) = Year2015.run xs
run ("2022" : xs) = Year2022.run xs
run (year : _) = error $ "Unknown year: " ++ year

main :: IO ()
main = getArgs >>= run
