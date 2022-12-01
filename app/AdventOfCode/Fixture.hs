module AdventOfCode.Fixture where

readChallengeInput :: Int -> Int -> IO String
readChallengeInput year day = readFile ("inputs/" ++ show year ++ "/" ++ show day)

runChallenge :: Show a => Int -> Int -> Int -> (String -> a) -> IO ()
runChallenge year day part challenge = do
  input <- readChallengeInput year day
  let result = challenge input
  putStrLn ("Challenge " ++ show year ++ "-" ++ show day ++ "-" ++ show part ++ ": " ++ show result)
