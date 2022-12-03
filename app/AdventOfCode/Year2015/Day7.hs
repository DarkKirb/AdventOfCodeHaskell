module AdventOfCode.Year2015.Day7 where

import AdventOfCode.Fixture (runChallenge)
import Data.Bits (Bits (shiftR, xor), shiftL, (.&.))
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.Word (Word16)
import Debug.Trace (trace, traceShowId)
import Foreign ((.|.))
import GHC.Generics (Generic)
import Text.Parsec (lower, many1, parse, string, try)
import Text.Parsec.Char (char)
import Text.Parsec.Combinator (sepEndBy1)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim ((<|>))
import Text.Parsec.String (Parser)
import Util (parseNumber)

data Wire = Wire String | Literal Word16
  deriving (Show, Eq, Generic)

instance Hashable Wire

parseWireName :: Parser Wire
parseWireName = do
  name <- many1 lower
  return $ Wire name

parseLiteral :: Parser Wire
parseLiteral = do
  literal <- parseNumber
  if literal > 65535
    then error ("Literal " ++ show literal ++ " out of range for Word16")
    else return $ Literal $ fromIntegral literal

parseWire :: Parser Wire
parseWire = try parseWireName <|> parseLiteral

data Opcode
  = Identity Wire
  | LiteralOp Word16
  | And Wire Wire
  | Or Wire Wire
  | Lshift Wire Int
  | Rshift Wire Int
  | Not Wire
  deriving (Show, Eq)

data Instruction = Instruction Opcode Wire
  deriving (Show, Eq)

data DAG
  = DLiteral Word16
  | DAnd DAG DAG
  | DOr DAG DAG
  | DLshift DAG Int
  | DRshift DAG Int
  | DNot DAG
  deriving (Show, Eq)

parseShamt :: Parser Int
parseShamt = do
  literal <- parseNumber
  if literal > 15
    then error ("Literal " ++ show literal ++ " out of range for shift amount")
    else return literal

parseUnOP :: Parser Opcode
parseUnOP = do
  string "NOT "
  Not <$> parseWire

parseBinOPWWName :: Parser String
parseBinOPWWName = try (string " AND ") <|> string " OR "

parseBinOPWW :: Wire -> Parser Opcode
parseBinOPWW w1 = do
  opName <- parseBinOPWWName
  w2 <- parseWire
  return $ case opName of
    " AND " -> And w1 w2
    " OR " -> Or w1 w2
    _ -> error "Unexpected error"

parseBinOPWIName :: Parser String
parseBinOPWIName = try (string " LSHIFT ") <|> string " RSHIFT "

parseBinOPWI :: Wire -> Parser Opcode
parseBinOPWI w1 = do
  opName <- parseBinOPWIName
  shamt <- parseShamt
  return $ case opName of
    " LSHIFT " -> Lshift w1 shamt
    " RSHIFT " -> Rshift w1 shamt
    _ -> error "Unexpected error"

parseBinOP :: Parser Opcode
parseBinOP = do
  w1 <- parseWire
  try (parseBinOPWW w1) <|> try (parseBinOPWI w1) <|> return (Identity w1)

parseOpcode :: Parser Opcode
parseOpcode = try parseUnOP <|> parseBinOP

parseInstruction :: Parser Instruction
parseInstruction = do
  opc <- parseOpcode
  string " -> "
  Instruction opc <$> parseWire

inputParser :: Parser [Instruction]
inputParser = parseInstruction `sepEndBy1` char '\n'

parseInput :: String -> Either ParseError [Instruction]
parseInput = parse inputParser ""

reduced :: DAG -> Word16
reduced (DLiteral a) = a
reduced (DAnd a b) = reduced $ reduce $ DAnd (reduce a) (reduce b)
reduced (DOr a b) = reduced $ reduce $ DOr (reduce a) (reduce b)
reduced (DLshift a b) = reduced $ reduce $ DLshift (reduce a) b
reduced (DRshift a b) = reduced $ reduce $ DRshift (reduce a) b
reduced (DNot a) = reduced $ reduce $ DNot $ reduce a

reduce :: DAG -> DAG
reduce (DAnd a b) = DLiteral $ reduced a .&. reduced b
reduce (DOr a b) = DLiteral $ reduced a .|. reduced b
reduce (DLshift a b) = DLiteral $ reduced a `shiftL` b
reduce (DRshift a b) = DLiteral $ reduced a `shiftR` b
reduce (DNot a) = DLiteral $ reduced a `xor` 65535
reduce x = x

resolve :: [Instruction] -> HashMap Wire Opcode
resolve = foldr resolveOnce HashMap.empty
  where
    resolveOnce (Instruction opc binding) = HashMap.insert binding opc

resolvedRef :: HashMap Wire Opcode -> Wire -> Opcode
resolvedRef hm (Wire s) = hm ! Wire s
resolvedRef _ (Literal i) = LiteralOp i

lowerToDAG' :: Opcode -> HashMap Wire Opcode -> DAG
lowerToDAG' (Identity a) hm = lowerToDAG (hm `resolvedRef` a) hm
lowerToDAG' (LiteralOp x) _ = DLiteral x
lowerToDAG' (And a b) hm = DAnd (lowerToDAG (hm `resolvedRef` a) hm) (lowerToDAG (hm `resolvedRef` b) hm)
lowerToDAG' (Or a b) hm = DOr (lowerToDAG (hm `resolvedRef` a) hm) (lowerToDAG (hm `resolvedRef` b) hm)
lowerToDAG' (Lshift a b) hm = DLshift (lowerToDAG (hm `resolvedRef` a) hm) b
lowerToDAG' (Rshift a b) hm = DRshift (lowerToDAG (hm `resolvedRef` a) hm) b
lowerToDAG' (Not a) hm = DNot (lowerToDAG (hm `resolvedRef` a) hm)

lowerToDAG :: Opcode -> HashMap Wire Opcode -> DAG
lowerToDAG a b =
  let c = reduce $ lowerToDAG' (trace ("staring lowerToDAG (" ++ show a ++ ")") a) b
   in trace ("lowerToDAG (" ++ show a ++ ") hm -> " ++ show c) c

part1 :: [Instruction] -> Word16
part1 is =
  let hm = traceShowId (resolve is)
   in reduced $ lowerToDAG (hm ! Wire "a") hm

run :: [String] -> IO ()
run [] = mapM_ (\x -> run [x]) ["1"]
run ("1" : _) = runChallenge 2015 7 1 parseInput part1
run (part : _) = error $ "Unknown part: " ++ part
