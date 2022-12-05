module AdventOfCode.Year2022 where

import qualified AdventOfCode.Year2022.Day1 as Day1
import qualified AdventOfCode.Year2022.Day2 as Day2
import qualified AdventOfCode.Year2022.Day3 as Day3
import qualified AdventOfCode.Year2022.Day4 as Day4
import qualified AdventOfCode.Year2022.Day5 as Day5

run :: [String] -> IO ()
run [] =
  mapM_
    (\x -> run [x])
    [ "1",
      "2",
      "3",
      "4",
      "5"
    ]
run ("1" : xs) = Day1.run xs
run ("2" : xs) = Day2.run xs
run ("3" : xs) = Day3.run xs
run ("4" : xs) = Day4.run xs
run ("5" : xs) = Day5.run xs
run (day : _) = error $ "Unknown day: " ++ day
