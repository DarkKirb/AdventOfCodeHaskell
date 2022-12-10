module AdventOfCode.Year2022 where

import qualified AdventOfCode.Year2022.Day1 as Day1
import qualified AdventOfCode.Year2022.Day10 as Day10
import qualified AdventOfCode.Year2022.Day2 as Day2
import qualified AdventOfCode.Year2022.Day3 as Day3
import qualified AdventOfCode.Year2022.Day4 as Day4
import qualified AdventOfCode.Year2022.Day5 as Day5
import qualified AdventOfCode.Year2022.Day6 as Day6
import qualified AdventOfCode.Year2022.Day7 as Day7
import qualified AdventOfCode.Year2022.Day8 as Day8
import qualified AdventOfCode.Year2022.Day9 as Day9

run :: [String] -> IO ()
run [] =
  mapM_
    (\x -> run [x])
    [ "1",
      "2",
      "3",
      "4",
      "5",
      "6",
      "7",
      "8",
      "9",
      "10"
    ]
run ("1" : xs) = Day1.run xs
run ("2" : xs) = Day2.run xs
run ("3" : xs) = Day3.run xs
run ("4" : xs) = Day4.run xs
run ("5" : xs) = Day5.run xs
run ("6" : xs) = Day6.run xs
run ("7" : xs) = Day7.run xs
run ("8" : xs) = Day8.run xs
run ("9" : xs) = Day9.run xs
run ("10" : xs) = Day10.run xs
run (day : _) = error $ "Unknown day: " ++ day
