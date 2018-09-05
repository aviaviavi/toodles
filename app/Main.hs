{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Control.Exception          as E
import           Data.Maybe
import qualified Data.Text                  as T
import           Data.Void
import           System.IO.HVFS
import           System.Path
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           System.Console.CmdArgs
import System.Console.CmdArgs.Explicit


import           Lib

data CommentedLine = CommentedLine

  { sourceFile  :: FilePath
  , lineNumber  :: Integer
  , rawText     :: T.Text
  , code        :: Maybe T.Text
  , commentText :: T.Text
  } deriving (Show)

data TodoEntry = TodoEntry
  { body     :: T.Text
  , assignee :: Maybe T.Text
  } deriving (Show)

data SourceFile = SourceFile {
  fullPath    :: FilePath,
  sourceLines :: [T.Text]
                             }

data PileArgs = PileArgs {
  projectRoot :: Maybe FilePath
                         } deriving (Show, Data, Typeable, Eq)

argParser :: PileArgs =
  PileArgs {
  projectRoot = def &= typFile &= help "Root directory of your project"
           }

fileTypeToComment :: [(T.Text, T.Text)]
fileTypeToComment =
  [ ("hs", "--")
  , ("js", "//")
  , ("yaml", "#")
  , ("go", "//")
  , (".proto", "//")
  , (".ts", "//")
  , (".py", "#")
  , (".java", "//")
  , (".cpp", "//")
  ]

unkownMarker :: T.Text
unkownMarker = "------UNKNOWN-DELIMETER-----"

getCommentForFileType :: T.Text -> T.Text
getCommentForFileType extension = fromMaybe unkownMarker $ lookup extension fileTypeToComment

type Parser = Parsec Void T.Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: T.Text -> Parser T.Text
symbol = L.symbol space

parseComment :: T.Text -> Parser T.Text
parseComment extension = do
  _ <- manyTill anyChar (symbol $ getCommentForFileType extension)
  b <- many anyChar
  return $ T.pack b

parseTodo :: T.Text -> Parser TodoEntry
parseTodo extension = do
  _ <- manyTill anyChar (symbol $ getCommentForFileType extension)
  _ <- symbol "TODO"
  b <- many anyChar
  return $ TodoEntry (T.pack b) Nothing

-- TODO hi hi hi

-- TODO(avi) here's a todo!
getAllFiles :: FilePath -> IO [SourceFile]
getAllFiles path = E.catch
  (do
    putStrLn path -- a comment!
    files <- recurseDir SystemFS path
    let validFiles = (filter fileHasValidExtension files)
    mapM_ putStrLn validFiles
    mapM (\f -> (SourceFile f) <$> (map T.pack . lines) <$> readFile f) validFiles)
  (\(e :: E.IOException) ->
     putStrLn ("Error reading " ++ path ++ ": " ++ show e) >> return [])

fileHasValidExtension :: FilePath -> Bool
fileHasValidExtension path =
  any (\ext -> ext `T.isSuffixOf` T.pack path)
  [".hs", ".yaml", ".go", "js", ".ts"]

getExtension :: FilePath -> T.Text
getExtension path = last $ T.splitOn "." (T.pack path)

runTodoParser :: SourceFile -> [TodoEntry]
runTodoParser (SourceFile path ls) =
  map fromJust $
  filter isJust $ map (parseMaybe . parseTodo $ getExtension path) ls

main :: IO ()
main = do
  putStrLn "pile"
  userArgs <- cmdArgs argParser
  let directory  = (projectRoot userArgs)
  if isJust directory then do
    allFiles <- getAllFiles $ fromJust directory
    let parsedTodos = concatMap runTodoParser allFiles
    print $ (take 5) parsedTodos
  else
    putStrLn "no directory supplied"
