module AdventOfCode.Year2022 where

import qualified AdventOfCode.Year2022.Day1 as Day1

run :: [String] -> IO ()
run [] =
  mapM_
    (\x -> run [x])
    [ "1"
    ]
run ("1" : xs) = Day1.run xs
run (day : _) = error $ "Unknown day: " ++ day
