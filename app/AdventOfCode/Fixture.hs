module AdventOfCode.Fixture where

readChallengeInput :: Int -> Int -> IO String
readChallengeInput year day = readFile ("inputs/" ++ show year ++ "/" ++ show day)

runTest :: (Show e, Show a, Eq a) => (String -> Either e i) -> (i -> a) -> String -> a -> IO ()
runTest parser challenge input expected = case parser input of
  Left e -> putStrLn $ "Error parsing test input: " ++ show e
  Right i ->
    let result = challenge i
     in if result == expected
          then putStrLn ("✅ " ++ input)
          else putStrLn ("❎ Input " ++ input ++ " failed (expected " ++ show expected ++ ", got " ++ show result)

runChallenge :: (Show e, Show a) => Int -> Int -> Int -> (String -> Either e i) -> (i -> a) -> IO ()
runChallenge year day part parser challenge = do
  input <- readChallengeInput year day
  case parser input of
    Left e -> putStrLn $ "Error running challenge " ++ show year ++ "-" ++ show day ++ "-" ++ show part ++ ": " ++ show e
    Right i -> putStrLn ("Challenge " ++ show year ++ "-" ++ show day ++ "-" ++ show part ++ ": " ++ show (challenge i))

parseNOP :: String -> Either () String
parseNOP = Right
