module AdventOfCode.Year2022 where

import qualified AdventOfCode.Year2022.Day1 as Day1
import qualified AdventOfCode.Year2022.Day2 as Day2

run :: [String] -> IO ()
run [] =
  mapM_
    (\x -> run [x])
    [ "1",
      "2"
    ]
run ("1" : xs) = Day1.run xs
run ("2" : xs) = Day2.run xs
run (day : _) = error $ "Unknown day: " ++ day
