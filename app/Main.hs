{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- TODO(avib|p=Just 2|#techdebt)  - break this into modules
module Main where

import qualified Control.Exception as E
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B8S
import Data.IORef
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Data.String.Utils
import qualified Data.Text as T
import Data.Version (showVersion)
import Data.Void
import Debug.Trace
import GHC.Generics
import Network.HTTP.Types (status200)
import Network.Wai
import Network.Wai.Handler.Warp
import Paths_toodles (version)
import Servant
import Servant.HTML.Blaze
import System.Console.CmdArgs
import System.IO.HVFS
import qualified System.IO.Strict as SIO
import System.Path
import System.Path.NameManip
import qualified Text.Blaze.Html5 as BZ
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Printf
import Text.Read

import Lib

type LineNumber = Integer

data TodoEntry
  = TodoEntryHead { id :: Integer
                  , body :: [T.Text]
                  , assignee :: Maybe T.Text
                  , sourceFile :: FilePath
                  , lineNumber :: LineNumber
                  , priority :: Maybe Integer
                  , customAttributes :: [(T.Text, T.Text)]
                  , tags :: [T.Text] }
  | TodoBodyLine T.Text
  deriving (Show, Generic)

data TodoListResult = TodoListResult
  { todos :: [TodoEntry]
  , message :: T.Text
  } deriving (Show, Generic)

data DeleteTodoRequest = DeleteTodoRequest
  { ids :: [Integer]
  } deriving (Show, Generic)

data EditTodoRequest = EditTodoRequest
  { editIds :: [Integer]
  , setAssignee :: Maybe T.Text
  , addTags :: [T.Text]
  } deriving (Show, Generic)

instance FromJSON TodoEntry

instance ToJSON TodoEntry

instance FromJSON TodoListResult

instance ToJSON TodoListResult

instance FromJSON DeleteTodoRequest

instance ToJSON DeleteTodoRequest

instance FromJSON EditTodoRequest

instance ToJSON EditTodoRequest

type ToodlesAPI
   = "todos" :> QueryFlag "recompute" :> Get '[ JSON] TodoListResult :<|> "todos" :> "delete" :> ReqBody '[ JSON] DeleteTodoRequest :> Post '[ JSON] T.Text :<|> "todos" :> "edit" :> ReqBody '[ JSON] EditTodoRequest :> Post '[ JSON] T.Text :<|> "static" :> Raw :<|> "source_file" :> Capture "id" Integer :> Get '[ HTML] BZ.Html :<|> Raw -- root html page

data ToodlesState = ToodlesState
  { results :: IORef TodoListResult
  }

toodlesAPI :: Proxy ToodlesAPI
toodlesAPI = Proxy

slice from to xs = take (to - from + 1) (drop from xs)

doUntilNull :: ([a] -> IO [a]) -> [a] -> IO ()
doUntilNull f xs = do
  result <- f xs
  if null result
    then return ()
    else doUntilNull f result

removeTodoFromCode :: MonadIO m => TodoEntry -> m ()
removeTodoFromCode = updateTodoLinesInFile (const [])

-- | Given a function to emit new lines for a given todo, write that update in
-- place of the current todo lines
updateTodoLinesInFile ::
     MonadIO m => (TodoEntry -> [T.Text]) -> TodoEntry -> m ()
updateTodoLinesInFile f todo = do
  let startIndex = lineNumber todo - 1
      newLines = map T.unpack $ f todo
  fileLines <- liftIO $ lines <$> (SIO.readFile $ sourceFile todo)
  let updatedLines =
        (slice 0 (fromIntegral $ startIndex - 1) fileLines) ++ newLines ++
        (slice
           ((fromIntegral startIndex) + (length $ body todo))
           (length fileLines - 1)
           fileLines)
  liftIO $ writeFile (sourceFile todo) $ unlines updatedLines

removeAndAdjust :: [TodoEntry] -> IO [TodoEntry]
removeAndAdjust deleteList =
  if null deleteList
    then return []
    else let deleteItem = head deleteList
             rest = tail deleteList
         in do _ <- removeTodoFromCode deleteItem
               return $
                 map
                   (\t ->
                      if (sourceFile t == sourceFile deleteItem) &&
                         (lineNumber t > lineNumber deleteItem)
                        then t
                             { lineNumber =
                                 (lineNumber t) -
                                 (fromIntegral . length $ body deleteItem)
                             }
                        else t)
                   rest

deleteTodos :: ToodlesState -> DeleteTodoRequest -> Handler T.Text
deleteTodos (ToodlesState ref) req = do
  refVal@(TodoListResult r _) <- liftIO $ readIORef ref
  let toDelete = filter (\t -> Main.id t `elem` (ids req)) r
  liftIO $ doUntilNull removeAndAdjust toDelete
  let updeatedResults =
        refVal
        { todos =
            (filter (\t -> not $ Main.id t `elem` (map Main.id toDelete)) r)
        }
  _ <-
    liftIO $ atomicModifyIORef' ref (const (updeatedResults, updeatedResults))
  return $ T.pack "{}"

editTodos :: ToodlesState -> EditTodoRequest -> Handler T.Text
editTodos (ToodlesState ref) req = do
  refVal@(TodoListResult r _) <- liftIO $ readIORef ref
  let editedList =
        map
          (\t ->
             if willEditTodo req t
               then (editTodo req t)
               else t)
          r
      editedFilteredList = filter (willEditTodo req) editedList
  _ <- mapM_ recordUpdates editedFilteredList
  return $ T.pack "{}"
  where
    willEditTodo :: EditTodoRequest -> TodoEntry -> Bool
    willEditTodo req entry = Main.id entry `elem` editIds req
    editTodo :: EditTodoRequest -> TodoEntry -> TodoEntry
    editTodo req entry =
      entry {assignee = (setAssignee req), tags = tags entry ++ (addTags req)}
    recordUpdates :: MonadIO m => TodoEntry -> m ()
    recordUpdates t = void $ updateTodoLinesInFile renderTodo t

renderTodo :: TodoEntry -> [T.Text]
renderTodo t =
  let comment =
        fromJust $ lookup ("." <> getExtension (sourceFile t)) fileTypeToComment
      detail =
        (T.pack "TODO(") <>
        (T.pack $
         Data.String.Utils.join
           "|"
           (map T.unpack $ [fromMaybe "" $ assignee t] ++
            [fmap (\p -> "p=" <> T.pack (show p)) priority t] ++
            (tags t) ++
            (map (\a -> fst a <> "=" <> snd a)) (customAttributes t))) <>
        (T.pack ") ")
      fullNoComments = mapHead (\l -> detail <> " - " <> l) $ body t
  in map (\l -> comment <> " " <> l) fullNoComments

mapHead :: (a -> a) -> [a] -> [a]
mapHead f (x:xs) = [f x] ++ xs
mapHead _ xs = xs

root :: Application
root _ res =
  readFile "./web/html/index.html" >>= \r ->
    res $ responseLBS status200 [] (B8S.pack r)

server :: ToodlesState -> Server ToodlesAPI
server s =
  liftIO . getFullSearchResults s :<|> deleteTodos s :<|> editTodos s :<|>
  serveDirectoryFileServer "web" :<|>
  showRawFile s :<|>
  return root

app :: ToodlesState -> Application
app s = (serve toodlesAPI) $ server s

isEntryHead :: TodoEntry -> Bool
isEntryHead (TodoEntryHead _ _ _ _ _ _ _ _) = True
isEntryHead _ = False

isBodyLine :: TodoEntry -> Bool
isBodyLine (TodoBodyLine _) = True
isBodyLine _ = False

combineTodo :: TodoEntry -> TodoEntry -> TodoEntry
combineTodo (TodoEntryHead i b a p n priority attrs tags) (TodoBodyLine l) =
  TodoEntryHead i (b ++ [l]) a p n priority attrs tags
combineTodo _ _ = error "Can't combine todoEntry of these types"

data SourceFile = SourceFile
  { fullPath :: FilePath
  , sourceLines :: [T.Text]
  } deriving (Show)

newtype AssigneeFilterRegex =
  AssigneeFilterRegex T.Text
  deriving (Show, Data, Eq)

data SearchFilter =
  AssigneeFilter AssigneeFilterRegex
  deriving (Show, Data, Eq)

data ToodlesArgs = ToodlesArgs
  { directory :: FilePath
  , assignee_search :: Maybe SearchFilter
  , limit_results :: Int
  , port :: Maybe Int
  } deriving (Show, Data, Typeable, Eq)

argParser :: ToodlesArgs
argParser =
  ToodlesArgs
  { directory = def &= typFile &= help "Root directory of your project"
  , assignee_search = def &= help "Filter todo's by assignee"
  , limit_results = def &= help "Limit number of search results"
  , port = def &= help "Run server on port"
  } &=
  summary ("toodles " ++ showVersion version) &=
  program "toodles" &=
  verbosity &=
  help "Manage TODO's directly from your codebase"

fileTypeToComment :: [(T.Text, T.Text)]
fileTypeToComment =
  [ (".c", "//")
  , (".clj", ";;")
  , (".cpp", "//")
  , (".ex", "#")
  , (".erl", "%")
  , (".go", "//")
  , (".hs", "--")
  , (".java", "//")
  , (".js", "//")
  , (".m", "//")
  , (".org", "")
  , (".proto", "//")
  , (".py", "#")
  , (".rb", "#")
  , (".rs", "//")
  , (".scala", "//")
  , (".sh", "#")
  , (".swift", "///")
  , (".ts", "//")
  , (".yaml", "#")
  ]

unkownMarker :: T.Text
unkownMarker = "UNKNOWN-DELIMETER-UNKNOWN-DELIMETER-UNKNOWN-DELIMETER"

getCommentForFileType :: T.Text -> T.Text
getCommentForFileType extension =
  fromMaybe unkownMarker $ lookup adjustedExtension fileTypeToComment
  where
    adjustedExtension =
      if T.isPrefixOf "." extension
        then extension
        else "." <> extension

type Parser = Parsec Void T.Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: T.Text -> Parser T.Text
symbol = L.symbol space

parseComment :: T.Text -> Parser TodoEntry
parseComment extension
  -- TODO(#bug|p=2) this will put mixed code/comment lines into a todo on a previous line
 = do
  _ <- manyTill anyChar (symbol $ getCommentForFileType extension)
  b <- many anyChar
  return . TodoBodyLine $ T.pack b

integer :: Parser Integer
integer = lexeme $ L.signed space L.decimal

parsePriority :: Parser Integer
parsePriority = do
  _ <- symbol "p="
  integer

parseAssignee :: Parser String
parseAssignee = many (noneOf [')', '|', '='])

-- TODO(p=3}|#techdebt) fix and type this better
parseDetails ::
     T.Text -> (Maybe T.Text, Maybe T.Text, [(T.Text, T.Text)], [T.Text])
parseDetails toParse =
  let tokens = T.splitOn "|" toParse
      a =
        find
          (\t ->
             (not (T.null t)) &&
             (not (T.isInfixOf "=" t) && (not (T.isPrefixOf "#" t))))
          tokens
      allDetails =
        map (\[a, b] -> (a, b)) $ filter (\t -> length t == 2) $
        map (T.splitOn "=") tokens
      priority = snd <$> (find (\t -> (T.strip $ fst t) == "p") allDetails)
      filteredDetails = filter (\t -> (T.strip $ fst t) /= "p") allDetails
      tags = filter (\t -> T.isPrefixOf "#" t) tokens
  in (a, priority, filteredDetails, tags)

inParens = between (symbol "(") (symbol ")")

stringToMaybe t =
  if T.null t
    then Nothing
    else Just t

fst4 (x, _, _, _) = x

snd4 (_, x, _, _) = x

thd4 (_, _, x, _) = x

fth4 (_, _, _, x) = x

prefixParserForFileType extension =
  let comment = symbol . getCommentForFileType $ extension
      orgMode =
        (try $ symbol "****") <|> (try $ symbol "***") <|> (try $ symbol "**") <|>
        (try $ symbol "*") <|>
        (symbol "-")
  in if extension == "org"
       then orgMode
       else comment

parseTodoEntryHead :: FilePath -> LineNumber -> Parser TodoEntry
parseTodoEntryHead path lineNum = do
  _ <- manyTill anyChar (prefixParserForFileType $ getExtension path)
  _ <- symbol "TODO"
  details <- optional $ try (inParens $ many (noneOf [')', '(']))
  let parsedDetails = parseDetails . T.pack <$> details
      priority = (readMaybe . T.unpack) =<< (snd4 =<< parsedDetails)
      otherDetails = maybe [] thd4 parsedDetails
      tags = maybe [] fth4 parsedDetails
  _ <- optional $ symbol "-"
  _ <- optional $ symbol ":"
  b <- many anyChar
  return $
    TodoEntryHead
      0
      [T.pack b]
      (stringToMaybe . T.strip $ fromMaybe "" (fst4 =<< parsedDetails))
      path
      lineNum
      priority
      otherDetails
      tags

parseTodo :: FilePath -> LineNumber -> Parser TodoEntry
parseTodo path lineNum =
  try (parseTodoEntryHead path lineNum) <|> (parseComment $ getExtension path)

getAllFiles :: FilePath -> IO [SourceFile]
getAllFiles path =
  E.catch
    (do putStrLn $ printf "Running toodles for path: %s" path
        files <- recurseDir SystemFS path
        let validFiles = filter isValidFile files
    -- TODO(p=3|avi|#techdebt) make sure it's a file first
        mapM
          (\f ->
             SourceFile f . (map T.pack . lines) <$>
             E.catch
               (SIO.readFile f)
               (\(e :: E.IOException) -> print e >> return ""))
          validFiles)
    (\(e :: E.IOException) ->
       putStrLn ("Error reading " ++ path ++ ": " ++ show e) >> return [])

fileHasValidExtension :: FilePath -> Bool
fileHasValidExtension path =
  any (\ext -> ext `T.isSuffixOf` T.pack path) (map fst fileTypeToComment)

-- TODO(avi|p=2|#feature|#techdebt|key=other.value) this should be configurable
ignoreFile :: FilePath -> Bool
ignoreFile file =
  let p = T.pack file
  in T.isInfixOf "node_modules" p || T.isSuffixOf "pb.go" p ||
     T.isSuffixOf "_pb2.py" p

getExtension :: FilePath -> T.Text
getExtension path = last $ T.splitOn "." (T.pack path)

isValidFile :: FilePath -> Bool
isValidFile f = fileHasValidExtension f && not (ignoreFile f)

runTodoParser :: SourceFile -> [TodoEntry]
runTodoParser (SourceFile path ls) =
  let parsedTodoLines =
        map
          (\(lineNum, lineText) -> parseMaybe (parseTodo path lineNum) lineText)
          (zip [1 ..] ls)
      groupedTodos = foldl foldTodoHelper ([], False) parsedTodoLines
  in fst groupedTodos

foldTodoHelper :: ([TodoEntry], Bool) -> Maybe TodoEntry -> ([TodoEntry], Bool)
foldTodoHelper (todos :: [TodoEntry], currentlyBuildingTodoLines :: Bool) maybeTodo
  | isNothing maybeTodo = (todos, False)
  | isEntryHead $ fromJust maybeTodo = (todos ++ [fromJust maybeTodo], True)
  | isBodyLine (fromJust maybeTodo) && currentlyBuildingTodoLines =
    (init todos ++ [combineTodo (last todos) (fromJust maybeTodo)], True)
  | otherwise = (todos, False)

prettyFormat :: TodoEntry -> String
prettyFormat (TodoEntryHead _ l a p n priority _ _) =
  printf
    "Assignee: %s\n%s%s:%d\n%s"
    (fromMaybe "None" a)
    (maybe "" (\x -> "Priority: " ++ show x ++ "\n") priority)
    p
    n
    (unlines $ map T.unpack l)
prettyFormat (TodoBodyLine _) = error "Invalid type for prettyFormat"

filterSearch :: Maybe SearchFilter -> (TodoEntry -> Bool)
filterSearch Nothing = const True
filterSearch (Just (AssigneeFilter (AssigneeFilterRegex query))) =
  \entry -> fromMaybe "" (assignee entry) == query

limitSearch :: [TodoEntry] -> Int -> [TodoEntry]
limitSearch results limit =
  if limit == 0
    then results
    else take limit results

runFullSearch :: ToodlesArgs -> IO TodoListResult
runFullSearch userArgs =
  let projectRoot = directory userArgs
  in do allFiles <- getAllFiles projectRoot
        let parsedTodos = concatMap runTodoParser allFiles
            filteredTodos =
              filter (filterSearch (assignee_search userArgs)) parsedTodos
            results = limitSearch filteredTodos $ limit_results userArgs
            indexedResults =
              map (\(i, r) -> r {Main.id = i}) $ zip [1 ..] results
        return $ TodoListResult indexedResults ""

getFullSearchResults :: ToodlesState -> Bool -> IO TodoListResult
getFullSearchResults (ToodlesState ref) recompute =
  if recompute
    then do
      putStrLn "refreshing todo's"
      userArgs <- cmdArgs argParser >>= setAbsolutePath
      sResults <- runFullSearch userArgs
      writtenResults <- atomicModifyIORef' ref (const (sResults, sResults))
      return writtenResults
    else putStrLn "cached read" >> readIORef ref

showRawFile :: ToodlesState -> Integer -> Handler BZ.Html
showRawFile (ToodlesState ref) entryId = do
  (TodoListResult r _) <- liftIO $ readIORef ref
  let entry = find (\t -> Main.id t == entryId) r
  liftIO $
    maybe
      (return "Not found")
      (\e -> addAnchors <$> readFile (sourceFile e))
      entry

addAnchors :: String -> BZ.Html
addAnchors s =
  let sourceLines = zip [1 ..] $ lines s
  in BZ.preEscapedToHtml $
     (unlines $
      map
        (\(i, l) -> printf "<pre><a name=\"line-%s\">%s</a></pre>" (show i) l)
        sourceLines)

setAbsolutePath :: ToodlesArgs -> IO ToodlesArgs
setAbsolutePath args =
  let pathOrDefault =
        if T.null . T.pack $ directory args
          then "."
          else directory args
  in do absolute <- normalise_path <$> absolute_path pathOrDefault
        return $ args {directory = absolute}

main :: IO ()
main = do
  userArgs <- cmdArgs argParser >>= setAbsolutePath
  sResults <- runFullSearch userArgs
  if isJust $ port userArgs
    then do
      let webPort = fromJust $ port userArgs
      ref <- newIORef sResults
      putStrLn $ "serving on " ++ show webPort
      run webPort $ app $ ToodlesState ref
    else mapM_ (putStrLn . prettyFormat) $ todos sResults
  return ()
