module AdventOfCode.Year2022.Day11 where

import AdventOfCode.Fixture (runChallenge)
import Control.Applicative (Alternative (many), (<|>))
import Control.Lens (ix, (%=), (+=), (.=), (^.))
import Control.Lens.Operators ((^?!))
import Control.Lens.TH (makeLenses)
import Control.Monad (forM_)
import Control.Monad.Trans.State (State, evalState, get)
import Data.List (sortBy)
import Data.Sequence (Seq ((:<|)), (|>))
import qualified Data.Sequence as Seq
import Text.Parsec (char, sepEndBy1, string)
import Text.Parsec.Combinator (sepBy1)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim (parse)
import Text.Parsec.String (Parser)
import Util (applyNM, parseNumber)

data Operand = Old | Literal Int
  deriving (Show)

applyOperand :: Operand -> Int -> Int
applyOperand Old o = o
applyOperand (Literal i) _ = i

data Expression = Add Operand Operand | Multiply Operand Operand
  deriving (Show)

applyExpression :: Expression -> Int -> Int
applyExpression (Add lhs rhs) o = applyOperand lhs o + applyOperand rhs o
applyExpression (Multiply lhs rhs) o = applyOperand lhs o * applyOperand rhs o

data Monkey = Monkey
  { _worryLevels :: Seq Int,
    _operation :: Expression,
    _divTest :: Int,
    _targetIfDivisible :: Int,
    _targetOtherwise :: Int,
    _inspectCount :: Int
  }
  deriving (Show)

makeLenses ''Monkey

oldParser :: Parser Operand
oldParser = string "old" >> return Old

literalParser :: Parser Operand
literalParser = Literal <$> parseNumber

operandParser :: Parser Operand
operandParser = oldParser <|> literalParser

expressionParser :: Parser Expression
expressionParser = do
  lhs <- operandParser
  char ' '
  operand <- char '+' <|> char '*'
  char ' '
  rhs <- operandParser
  return $ case operand of
    '+' -> Add lhs rhs
    '*' -> Multiply lhs rhs
    _ -> error "Invalid state"

commaSeparatedList :: Parser a -> Parser [a]
commaSeparatedList p = p `sepBy1` (char ',' >> many (char ' '))

monkeyParser :: Parser Monkey
monkeyParser = do
  string "Monkey "
  parseNumber
  string ":\n  Starting items: "
  _worryLevels <- commaSeparatedList parseNumber
  string "\n  Operation: new = "
  _operation <- expressionParser
  string "\n  Test: divisible by "
  _divTest <- parseNumber
  string "\n    If true: throw to monkey "
  _targetIfDivisible <- parseNumber
  string "\n    If false: throw to monkey "
  _targetOtherwise <- parseNumber
  char '\n'
  return $
    Monkey
      { _worryLevels = Seq.fromList _worryLevels,
        _operation = _operation,
        _divTest = _divTest,
        _targetIfDivisible = _targetIfDivisible,
        _targetOtherwise = _targetOtherwise,
        _inspectCount = 0
      }

inputParser :: Parser [Monkey]
inputParser = monkeyParser `sepEndBy1` char '\n'

parseInput :: String -> Either ParseError [Monkey]
parseInput = parse inputParser ""

type StateType = [Monkey]

type ResultType = Int

stepItem :: Bool -> Int -> Int -> State StateType ()
stepItem doPart2 modulus monkeyId = do
  monkeys <- get
  let monkey = monkeys ^?! ix monkeyId
  case monkey ^. worryLevels of
    Seq.Empty -> return ()
    (firstItem :<| otherItems) -> do
      ix monkeyId . worryLevels .= otherItems
      ix monkeyId . inspectCount += 1
      let newItem' = applyExpression (monkey ^. operation) firstItem
      let newItem = (if doPart2 then newItem' else newItem' `div` 3) `mod` modulus
      let targetId =
            if (newItem `mod` (monkey ^. divTest)) == 0
              then monkey ^. targetIfDivisible
              else monkey ^. targetOtherwise
      ix targetId . worryLevels %= (|> newItem)
  return ()

stepMonkey :: Bool -> Int -> Int -> State StateType ()
stepMonkey doPart2 modulus monkeyId = do
  monkeys <- get
  let monkey = monkeys ^?! ix monkeyId
  applyNM (length (monkey ^. worryLevels)) (stepItem doPart2 modulus monkeyId)

step :: Bool -> Int -> State StateType ()
step doPart2 modulus = do
  monkeys <- get
  forM_ [0 .. (length monkeys - 1)] (stepMonkey doPart2 modulus)

part1M :: State StateType StateType
part1M = do
  modulus <- lcmOfTests <$> get
  applyNM 19 (step False modulus)
  get

part2M :: State StateType StateType
part2M = do
  modulus <- lcmOfTests <$> get
  applyNM 9999 (step True modulus)
  get

lcmOfTests :: StateType -> Int
lcmOfTests = foldr lcm 1 . fmap (^. divTest)

part1 :: StateType -> Int
part1 = product . take 2 . sortBy (flip compare) . fmap (^. inspectCount) . evalState part1M

part2 :: StateType -> Int
part2 = product . take 2 . sortBy (flip compare) . fmap (^. inspectCount) . evalState part2M

run :: [String] -> IO ()
run [] = mapM_ (\x -> run [x]) ["1", "2"]
run ("1" : _) = runChallenge 2022 11 1 parseInput part1
run ("2" : _) = runChallenge 2022 11 2 parseInput part2
run (part : _) = error $ "Unknown part: " ++ part
