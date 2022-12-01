module AdventOfCode.Year2015 where

import qualified AdventOfCode.Year2015.Day1 as Day1
import qualified AdventOfCode.Year2015.Day2 as Day2
import qualified AdventOfCode.Year2015.Day3 as Day3
-- import qualified AdventOfCode.Year2015.Day4 as Day4
import qualified AdventOfCode.Year2015.Day5 as Day5
import qualified AdventOfCode.Year2015.Day6 as Day6

run :: IO ()
run = do
  Day1.run
  Day2.run
  Day3.run
  --    >> Day4.run
  Day5.run
  Day6.run
