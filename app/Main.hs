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
import qualified Data.ByteString.Lazy.Char8 as B8S
import           Data.IORef
import           Data.Maybe
import           Data.List
import           Data.Monoid
import           Data.Proxy
import qualified Data.Text                  as T
import           Data.Version               (showVersion)
import           Data.Void
import           Debug.Trace
import Servant.HTML.Blaze
import           GHC.Generics
import  qualified Text.Blaze.Html5 as BZ
import           Network.HTTP.Types         (status200)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Paths_toodles              (version)
import           Servant
import           System.Console.CmdArgs
import           System.IO.HVFS
import qualified System.IO.Strict           as SIO
import           System.Path
import           System.Path.NameManip
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Printf

import           Lib

-- data CommentedLine = CommentedLine
--   { sourceFile  :: FilePath
--   , lineNumber  :: Integer
--   , commentText :: T.Text
--   } deriving (Show)

type LineNumber = Integer

data TodoEntry = TodoEntryHead
  {
    id         :: Integer
  , body       :: [T.Text]
  , assignee   :: Maybe T.Text
  , sourceFile :: FilePath
  , lineNumber :: LineNumber
  } | TodoBodyLine T.Text deriving (Show, Generic)

data TodoListResult = TodoListResult {
  todos   :: [TodoEntry],
  message :: T.Text
                                     } deriving (Show, Generic)

instance FromJSON TodoEntry
instance ToJSON TodoEntry
instance FromJSON TodoListResult
instance ToJSON TodoListResult

type ToodlesAPI =
  "todos" :> Get '[JSON] TodoListResult :<|>
  "static" :> Raw :<|> -- file server
  "source_file" :> Capture "id" Integer :> Get '[HTML] BZ.Html :<|> -- source file
  Raw -- root html page

data ToodlesState = ToodlesState {
  results :: IORef TodoListResult
                                 }

toodlesAPI :: Proxy ToodlesAPI
toodlesAPI = Proxy

root :: Application
root _ res = readFile "./web/html/index.html" >>= \r -> res $
  responseLBS
  status200
  []
  (B8S.pack r)

server :: ToodlesState -> Server ToodlesAPI
server s = liftIO (getFullSearchResults s) :<|>
  serveDirectoryFileServer "web" :<|>
  showRawFile s :<|>
  return root

app :: ToodlesState -> Application
app s = (serve toodlesAPI) $ server s

isEntryHead :: TodoEntry -> Bool
isEntryHead (TodoEntryHead _ _ _ _ _) = True
isEntryHead _                       = False

isBodyLine :: TodoEntry -> Bool
isBodyLine (TodoBodyLine _ ) = True
isBodyLine _                 = False

combineTodo :: TodoEntry -> TodoEntry -> TodoEntry
combineTodo (TodoEntryHead i b a p n) (TodoBodyLine l) = TodoEntryHead i (b ++ [l]) a p n
combineTodo _ _ = error "Can't combine todoEntry of these types"

data SourceFile = SourceFile
  { fullPath    :: FilePath
  , sourceLines :: [T.Text]
  } deriving (Show)

newtype AssigneeFilterRegex = AssigneeFilterRegex T.Text deriving (Show, Data, Eq)

data SearchFilter = AssigneeFilter AssigneeFilterRegex deriving (Show, Data, Eq)

data ToodlesArgs = ToodlesArgs
  { project_root    :: FilePath
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

parseTodoEntryHead :: FilePath -> LineNumber -> Parser TodoEntry
parseTodoEntryHead path lineNum = do
  _ <- manyTill anyChar (symbol . getCommentForFileType $ getExtension path)
  _ <- symbol "TODO"
  a <- optional $ try (inParens $ many (noneOf [')']))
  _ <- optional $ symbol "-"
  _ <- optional $ symbol ":"
  b <- many anyChar
  return $ TodoEntryHead 0 [T.pack b] (stringToMaybe . T.strip . T.pack $ fromMaybe "" a) path lineNum

parseTodo :: FilePath -> LineNumber -> Parser TodoEntry
parseTodo path lineNum = try (parseTodoEntryHead path lineNum) <|> (parseComment $ getExtension path)

getAllFiles :: FilePath -> IO [SourceFile]
getAllFiles path = E.catch
  (do
    putStrLn $ printf "Running toodles for path: %s" path
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
  let parsedTodoLines = map (\(lineNum, lineText) -> parseMaybe (parseTodo path lineNum) lineText) (zip [1..] ls)
      groupedTodos = foldl foldTodoHelper ([], False) parsedTodoLines in
    fst groupedTodos

foldTodoHelper :: ([TodoEntry], Bool) -> Maybe TodoEntry -> ([TodoEntry], Bool)
foldTodoHelper (todos :: [TodoEntry], currentlyBuildingTodoLines :: Bool) maybeTodo
  | isNothing maybeTodo = (todos, False)
  | isEntryHead $ fromJust maybeTodo = (todos ++ [fromJust maybeTodo], True)
  | isBodyLine (fromJust maybeTodo) && currentlyBuildingTodoLines =
    (init todos ++ [combineTodo (last todos) (fromJust maybeTodo)], True)
  | otherwise = (todos, False)

prettyFormat :: TodoEntry -> String
prettyFormat (TodoEntryHead _ l a p n) =
  printf "Assignee: %s\n%s:%d\n%s" (fromMaybe "None" a) p n (unlines $ map T.unpack l)
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
  let directory  = project_root userArgs in do
  allFiles <- getAllFiles directory
  let parsedTodos = concatMap runTodoParser allFiles
      filteredTodos = filter (filterSearch (assignee_search userArgs)) parsedTodos
      results = limitSearch filteredTodos $ limit_results userArgs
      indexedResults = map (\(i, r) -> r {Main.id = i}) $ zip [1..] results
  return $ TodoListResult indexedResults ""

getFullSearchResults :: ToodlesState -> IO TodoListResult
getFullSearchResults (ToodlesState ref) = putStrLn "reading results..." >> readIORef ref

showRawFile :: ToodlesState -> Integer -> Handler BZ.Html
showRawFile (ToodlesState ref) entryId = do
  (TodoListResult r _) <- liftIO $ readIORef ref
  let entry = find (\t -> Main.id t == entryId) r
  liftIO $ maybe (return "Not found") (\e -> addAnchors <$> readFile (sourceFile e)) entry

addAnchors :: String -> BZ.Html
addAnchors s =
  let sourceLines = zip [1..] $ lines s in
  BZ.preEscapedToHtml $ (unlines $ map (\(i, l) -> printf "<pre><a name=\"line-%s\">%s</a></pre>" (show i) l) sourceLines)

setAbsolutePath :: ToodlesArgs -> IO ToodlesArgs
setAbsolutePath args =
  let pathOrDefault = if T.null . T.pack $ project_root args then "." else project_root args in do
    absolute <- normalise_path <$> absolute_path pathOrDefault
    return $ args { project_root = absolute }

main :: IO ()
main = do
  userArgs <- cmdArgs argParser >>= setAbsolutePath
  sResults <- runFullSearch userArgs
  if runServer userArgs
    then do
      ref <- newIORef sResults
      putStrLn "serving on 9001"
      run 9001 $ app $ ToodlesState ref
     else
      mapM_ (putStrLn . prettyFormat) $ todos sResults
  return ()
