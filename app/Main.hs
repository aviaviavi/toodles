{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Main where

import qualified Control.Exception          as E
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.IORef
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Version               (showVersion)
import           Data.Void
import           Debug.Trace
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Paths_toodles              (version)
import           Servant
import           Servant.API
import           Servant.API
import           System.Console.CmdArgs
import           System.IO.HVFS
import qualified System.IO.Strict           as SIO
import           System.Path
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Printf

import           Lib

data CommentedLine = CommentedLine
  { sourceFile  :: FilePath
  , lineNumber  :: Integer
  , commentText :: T.Text
  } deriving (Show)

data TodoEntry = TodoEntryHead
  { body     :: [T.Text]
  , assignee :: Maybe T.Text
  } | TodoBodyLine T.Text deriving (Show, Generic)

data TodoListResult = TodoListResult {
  todos   :: [TodoEntry],
  message :: T.Text
                                     } deriving (Show, Generic)

instance FromJSON TodoEntry
instance ToJSON TodoEntry
instance FromJSON TodoListResult
instance ToJSON TodoListResult

type ToodlesAPI = "todos" :> Get '[JSON] TodoListResult

data ToodlesState = ToodlesState {
  results :: IORef TodoListResult
                                 }

toodlesAPI :: Proxy ToodlesAPI
toodlesAPI = Proxy

server :: ToodlesState -> Server ToodlesAPI
server s = liftIO $ getFullSearchResults s

app :: ToodlesState -> Application
app s = serve toodlesAPI $ server s

isEntryHead :: TodoEntry -> Bool
isEntryHead (TodoEntryHead _ _) = True
isEntryHead _                   = False

isBodyLine :: TodoEntry -> Bool
isBodyLine (TodoBodyLine _ ) = True
isBodyLine _                 = False

combineTodo :: TodoEntry -> TodoEntry -> TodoEntry
combineTodo (TodoEntryHead b a) (TodoBodyLine l) = TodoEntryHead (b ++ [l]) a
combineTodo _ _ = error "Can't combine todoEntry of these types"

data SourceFile = SourceFile {
  fullPath    :: FilePath,
  sourceLines :: [T.Text]
                             } deriving (Show)

newtype AssigneeFilterRegex = AssigneeFilterRegex T.Text deriving (Show, Data, Eq)

data SearchFilter = AssigneeFilter AssigneeFilterRegex deriving (Show, Data, Eq)

data ToodlesArgs = ToodlesArgs
  { project_root    :: Maybe FilePath
  , assignee_search :: Maybe SearchFilter
  , limit_results   :: Int
  , runServer       :: Bool
  } deriving (Show, Data, Typeable, Eq)

argParser :: ToodlesArgs
argParser =
  ToodlesArgs
  {project_root = def &= typFile &= help "Root directory of your project"
  , assignee_search = def &= help "Filter todo's by assignee"
  , limit_results = def &= help "Limit number of search results"
  , runServer = def &= help "Run server"
  } &=
  summary ("toodles " ++ showVersion version) &=
  program "toodles" &=
  verbosity &=
  help "Manage TODO's directly from your codebase"

-- TODO(avi) add more languages
fileTypeToComment :: [(T.Text, T.Text)]
fileTypeToComment =
  [ (".cpp", "//")
  , (".go", "//")
  , (".hs", "--")
  , (".java", "//")
  , (".js", "//")
  , (".proto", "//")
  , (".py", "#")
  , (".rb", "#")
  , (".scala", "//")
  , (".sh", "#")
  , (".ts", "//")
  , (".yaml", "#")
  ]

unkownMarker :: T.Text
unkownMarker = "UNKNOWN-DELIMETER-UNKNOWN-DELIMETER-UNKNOWN-DELIMETER"

getCommentForFileType :: T.Text -> T.Text
getCommentForFileType extension =
  fromMaybe unkownMarker $ lookup adjustedExtension fileTypeToComment where
    adjustedExtension = if T.isPrefixOf "." extension then extension else "." <> extension

type Parser = Parsec Void T.Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: T.Text -> Parser T.Text
symbol = L.symbol space

parseComment :: T.Text -> Parser TodoEntry
parseComment extension = do
  _ <- manyTill anyChar (symbol $ getCommentForFileType extension)
  b <- many anyChar
  return . TodoBodyLine $ T.pack b

inParens = between (symbol "(") (symbol ")")

stringToMaybe t = if T.null t then Nothing else Just t

parseTodoEntryHead :: T.Text -> Parser TodoEntry
parseTodoEntryHead extension = do
  _ <- manyTill anyChar (symbol $ getCommentForFileType extension)
  _ <- symbol "TODO"
  a <- optional $ try (inParens $ many (noneOf [')']))
  _ <- optional $ symbol "-"
  _ <- optional $ symbol ":"
  b <- many anyChar
  return $ TodoEntryHead [T.pack b] $ stringToMaybe . T.strip . T.pack $ fromMaybe "" a

parseTodo :: T.Text -> Parser TodoEntry
parseTodo ext = try (parseTodoEntryHead ext) <|> parseComment ext

getAllFiles :: FilePath -> IO [SourceFile]
getAllFiles path = E.catch
  (do
    putStrLn path
    files <- recurseDir SystemFS path
    let validFiles = filter isValidFile files
    -- TODO make sure it's a file first
    mapM (\f -> SourceFile f . (map T.pack . lines) <$> E.catch (SIO.readFile f) (\(e :: E.IOException) -> print e >> return "")) validFiles)
  (\(e :: E.IOException) ->
     putStrLn ("Error reading " ++ path ++ ": " ++ show e) >> return [])

fileHasValidExtension :: FilePath -> Bool
fileHasValidExtension path =
  any (\ext -> ext `T.isSuffixOf` T.pack path) (map fst fileTypeToComment)

-- TODO(avi) this should be configurable
ignoreFile :: FilePath -> Bool
ignoreFile file = let p = T.pack file in
  T.isInfixOf "node_modules" p || T.isSuffixOf "pb.go" p || T.isSuffixOf "_pb2.py" p

getExtension :: FilePath -> T.Text
getExtension path = last $ T.splitOn "." (T.pack path)

isValidFile :: FilePath -> Bool
isValidFile f = fileHasValidExtension f && not (ignoreFile f)

runTodoParser :: SourceFile -> [TodoEntry]
runTodoParser (SourceFile path ls) =
  let parsedTodoLines = map (parseMaybe . parseTodo $ getExtension path) ls
      groupedTodos = foldl foldFn ([], False) parsedTodoLines in
    fst groupedTodos

-- TODO(avi) this needs a better name
foldFn :: ([TodoEntry], Bool) -> Maybe TodoEntry -> ([TodoEntry], Bool)
foldFn (todos :: [TodoEntry], currentlyBuildingTodoLines :: Bool) maybeTodo
  | isNothing maybeTodo = (todos, False)
  | isEntryHead $ fromJust maybeTodo = (todos ++ [fromJust maybeTodo], True)
  | isBodyLine (fromJust maybeTodo) && currentlyBuildingTodoLines =
    (init todos ++ [combineTodo (last todos) (fromJust maybeTodo)], True)
  | otherwise = (todos, False)

prettyFormat :: TodoEntry -> String
prettyFormat (TodoEntryHead l a) =
  printf "Assignee: %s\n%s" (fromMaybe "None" a) (unlines $ map T.unpack l)
prettyFormat (TodoBodyLine _) = error "Invalid type for prettyFormat"

filterSearch :: Maybe SearchFilter -> (TodoEntry -> Bool)
filterSearch Nothing = const True
filterSearch (Just (AssigneeFilter (AssigneeFilterRegex query))) =
  \entry -> fromMaybe "" (assignee entry) == query

limitSearch :: [TodoEntry] -> Int -> [TodoEntry]
limitSearch results limit =
  if limit == 0 then results else take limit results

runFullSearch :: ToodlesArgs -> IO TodoListResult
runFullSearch userArgs =
  let directory  = project_root userArgs in
  if isJust directory then do
    allFiles <- getAllFiles $ fromJust directory
    let parsedTodos = concatMap runTodoParser allFiles
        filteredTodos = filter (filterSearch (assignee_search userArgs)) parsedTodos
        results = limitSearch filteredTodos $ limit_results userArgs
    return $ TodoListResult results ""
  else do
    putStrLn "no directory supplied"
    return $ TodoListResult [] "no directory supplied"

getFullSearchResults :: ToodlesState -> IO TodoListResult
getFullSearchResults (ToodlesState ref) = readIORef ref

main :: IO ()
main = do
  userArgs <- cmdArgs argParser
  sResults <- runFullSearch userArgs
  if runServer userArgs
    then do
      ref <- newIORef sResults
      run 9001 $ app $ ToodlesState ref
     else
      mapM_ (putStrLn . prettyFormat) $ todos sResults
  return ()
