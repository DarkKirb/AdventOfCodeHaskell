module AdventOfCode.Fixture where

readChallengeInput :: Int -> Int -> IO String
readChallengeInput year day = readFile ("inputs/" ++ show year ++ "/" ++ show day)

runChallenge :: (Show e, Show a) => Int -> Int -> Int -> (String -> Either e i) -> (i -> a) -> IO ()
runChallenge year day part parser challenge = do
  input <- readChallengeInput year day
  case parser input of
    Left e -> putStrLn $ "Error running challenge " ++ show year ++ "-" ++ show day ++ "-" ++ show part ++ ": " ++ show e
    Right i -> putStrLn ("Challenge " ++ show year ++ "-" ++ show day ++ "-" ++ show part ++ ": " ++ show (challenge i))

parseNOP :: String -> Either () String
parseNOP = Right
