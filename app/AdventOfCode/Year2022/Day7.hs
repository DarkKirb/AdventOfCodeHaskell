module AdventOfCode.Year2022.Day7 where

import AdventOfCode.Fixture (runChallenge)
import Data.Bifunctor (first)
import Data.Maybe (fromMaybe, mapMaybe)
import Text.Parsec (char, endBy1, parse, sepEndBy1, string, try)
import Text.Parsec.Char (noneOf)
import Text.Parsec.Combinator (many1)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim ((<|>))
import Text.Parsec.String (Parser)
import Util (parseNumber)

data File = Directory String | File {size :: Int, name :: String}
  deriving (Show)

getSize :: File -> Maybe Int
getSize (File {size}) = Just size
getSize _ = Nothing

getName :: File -> String
getName (File {name}) = name
getName (Directory name) = name

data Command = ChangeDir String | List [File]
  deriving (Show)

data DirTree = FileNode String Int | DirectoryNode String [DirTree]
  deriving (Show)

getNameTree :: DirTree -> String
getNameTree (FileNode name _) = name
getNameTree (DirectoryNode name _) = name

newTree :: DirTree
newTree = DirectoryNode "" []

insert :: String -> Int -> DirTree -> DirTree
insert fname size (DirectoryNode dname fs) = DirectoryNode dname (FileNode fname size : fs)
insert _ _ _ = error "Cannot insert into file"

get :: String -> DirTree -> Maybe DirTree
get fname (DirectoryNode dname (f : fs))
  | fname == getNameTree f = Just f
  | otherwise = get fname (DirectoryNode dname fs)
get _ _ = Nothing

getOr :: DirTree -> String -> DirTree -> DirTree
getOr def fname dt = fromMaybe def $ get fname dt

remove :: String -> DirTree -> DirTree
remove fname (DirectoryNode dname (f : fs))
  | fname == getNameTree f = DirectoryNode dname fs
  | otherwise = case remove fname (DirectoryNode dname fs) of
      DirectoryNode _ fs' -> DirectoryNode dname (f : fs')
      _ -> error "Unexpected return value"
remove _ n = n

insertRec :: [String] -> Int -> DirTree -> DirTree
insertRec [fname] size dt = insert fname size dt
insertRec (dname : fs) size dt =
  let curNode = getOr (DirectoryNode dname []) dname dt
      inserted = insertRec fs size curNode
      removed = remove dname dt
   in case removed of
        DirectoryNode dname' fs' -> DirectoryNode dname' (inserted : fs')
        _ -> error "Unexpected return value"
insertRec _ _ dt = dt

lsDirParser :: Parser File
lsDirParser = do
  string "dir "
  Directory <$> many1 (noneOf "\n")

lsFileParser :: Parser File
lsFileParser = do
  fileSize <- parseNumber
  char ' '
  fileName <- many1 (noneOf "\n")
  return $ File {size = fileSize, name = fileName}

fileParser :: Parser File
fileParser = try lsDirParser <|> lsFileParser

lsResponseParser :: Parser [File]
lsResponseParser = fileParser `endBy1` char '\n'

lsParser :: Parser Command
lsParser = do
  string "ls\n"
  List <$> lsResponseParser

cdParser :: Parser Command
cdParser = do
  string "cd "
  target <- many1 (noneOf "\n")
  char '\n'
  return $ ChangeDir target

commandParser :: Parser Command
commandParser = char ' ' >> (try lsParser <|> cdParser)

inputParser :: Parser [Command]
inputParser = char '$' >> (commandParser `sepEndBy1` char '$')

parseInput :: String -> Either ParseError [Command]
parseInput = parse inputParser ""

resolveFileSizes :: [Command] -> [([String], Int)]
resolveFileSizes = fmap (first reverse) . resolveFileSizes' []
  where
    fileToFileSize :: [String] -> File -> Maybe ([String], Int)
    fileToFileSize ds f = do
      size <- getSize f
      return $ (getName f : ds, size)
    filesToFileSizes :: [String] -> [File] -> [([String], Int)]
    filesToFileSizes ds = mapMaybe (fileToFileSize ds)
    resolveFileSizes' _ [] = []
    resolveFileSizes' _ (ChangeDir "/" : xs) = resolveFileSizes' [] xs
    resolveFileSizes' (_ : ds) (ChangeDir ".." : xs) = resolveFileSizes' ds xs
    resolveFileSizes' ds (ChangeDir d : xs) = resolveFileSizes' (d : ds) xs
    resolveFileSizes' ds (List l : xs) = filesToFileSizes ds l ++ resolveFileSizes' ds xs

resolvedToTree :: [([String], Int)] -> DirTree
resolvedToTree = foldr (uncurry insertRec) newTree

allDirSizes :: DirTree -> [Int]
allDirSizes (FileNode _ s) = [s]
allDirSizes (DirectoryNode _ []) = [0]
allDirSizes (DirectoryNode _ (x : xs)) =
  let (tailDirSize : otherSizes) = allDirSizes (DirectoryNode "" xs)
      (subDirSize : otherSubSizes) = allDirSizes x
   in case x of
        DirectoryNode _ _ -> tailDirSize + subDirSize : subDirSize : otherSizes ++ otherSubSizes
        _ -> tailDirSize + subDirSize : otherSizes ++ otherSubSizes

commonPart :: [Command] -> [Int]
commonPart = allDirSizes . resolvedToTree . resolveFileSizes

part1 :: [Command] -> Int
part1 = sum . filter (<= 100000) . commonPart

part2 :: [Command] -> Int
part2 input =
  let sizes = commonPart input
      freeSpace = 70000000 - head sizes
      spaceToFree = 30000000 - freeSpace
   in minimum $ filter (>= spaceToFree) sizes

run :: [String] -> IO ()
run [] = mapM_ (\x -> run [x]) ["1", "2"]
run ("1" : _) = runChallenge 2022 7 1 parseInput part1
run ("2" : _) = runChallenge 2022 7 2 parseInput part2
run (part : _) = error $ "Unknown part: " ++ part
