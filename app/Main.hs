{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TypeOperators       #-}

-- TODO(avi|p=3|#cleanup|key=val|k3y=asdf) - break this into modules
module Main where

import qualified Control.Exception          as E
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString.Char8      as B8SS
import           Data.Either
import           Data.IORef
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import           Data.String.Utils
import qualified Data.Text                  as T
import           Data.Version               (showVersion)
import           Data.Void
import qualified Data.Yaml                  as Y
import           GHC.Generics
import           Network.Wai.Handler.Warp
import           Paths_toodles
import           Servant
import           Servant.HTML.Blaze
import           System.Console.CmdArgs
import           System.Directory
import           System.IO.HVFS
import qualified System.IO.Strict           as SIO
import           System.Path
import           System.Path.NameManip
import qualified Text.Blaze.Html5           as BZ
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Printf
import           Text.Read
import           Text.Regex.Posix

type LineNumber = Integer

data TodoEntry
  = TodoEntryHead { id               :: Integer
                  , body             :: [T.Text]
                  , assignee         :: Maybe T.Text
                  , sourceFile       :: FilePath
                  , lineNumber       :: LineNumber
                  , priority         :: Maybe Integer
                  , customAttributes :: [(T.Text, T.Text)]
                  , tags             :: [T.Text]
                  , leadingText      :: T.Text }
  | TodoBodyLine T.Text
  deriving (Show, Generic)

data TodoListResult = TodoListResult
  { todos   :: [TodoEntry]
  , message :: T.Text
  } deriving (Show, Generic)

newtype DeleteTodoRequest = DeleteTodoRequest
  { ids :: [Integer]
  } deriving (Show, Generic)

data EditTodoRequest = EditTodoRequest
  { editIds     :: [Integer]
  , setAssignee :: Maybe T.Text
  , addTags     :: [T.Text]
  , addKeyVals  :: [(T.Text, T.Text)]
  , setPriority :: Maybe Integer
  } deriving (Show, Generic)

newtype ToodlesConfig = ToodlesConfig {
  ignore :: [FilePath]
                                   } deriving (Show, Generic)

instance FromJSON TodoEntry

instance ToJSON TodoEntry

instance FromJSON TodoListResult

instance ToJSON TodoListResult

instance FromJSON DeleteTodoRequest

instance ToJSON DeleteTodoRequest

instance FromJSON EditTodoRequest

instance ToJSON EditTodoRequest

instance FromJSON ToodlesConfig

type ToodlesAPI =
  "todos" :> QueryFlag "recompute" :> Get '[ JSON] TodoListResult :<|>
  "todos" :> "delete" :> ReqBody '[ JSON] DeleteTodoRequest :> Post '[ JSON] T.Text :<|>
  "todos" :> "edit" :> ReqBody '[ JSON] EditTodoRequest :> Post '[ JSON] T.Text :<|>
  "static" :> Raw :<|>
  "source_file" :> Capture "id" Integer :> Get '[ HTML] BZ.Html :<|>
  CaptureAll "anything-else" T.Text :> Get '[HTML] BZ.Html

server :: ToodlesState -> Server ToodlesAPI
server s =
  liftIO . getFullSearchResults s :<|>
  deleteTodos s :<|>
  editTodos s :<|>
  serveDirectoryFileServer (dataPath s) :<|>
  showRawFile s :<|>
  root s

data ToodlesState = ToodlesState
  { results  :: IORef TodoListResult,
    dataPath :: FilePath
  }

toodlesAPI :: Proxy ToodlesAPI
toodlesAPI = Proxy

slice :: Int -> Int -> [a] -> [a]
slice f t xs = take (t - f + 1) (drop f xs)

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
  fileLines <- liftIO $ lines <$> SIO.readFile (sourceFile todo)
  let updatedLines =
        slice 0 (fromIntegral $ startIndex - 1) fileLines ++ newLines ++
        (slice
           (fromIntegral startIndex + length (body todo))
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
                                 lineNumber t -
                                 (fromIntegral . length $ body deleteItem)
                             }
                        else t)
                   rest

deleteTodos :: ToodlesState -> DeleteTodoRequest -> Handler T.Text
deleteTodos (ToodlesState ref _) req = do
  refVal@(TodoListResult r _) <- liftIO $ readIORef ref
  let toDelete = filter (\t -> Main.id t `elem` ids req) r
  liftIO $ doUntilNull removeAndAdjust toDelete
  let updeatedResults =
        refVal
        { todos =
            filter (\t -> Main.id t `notElem` map Main.id toDelete) r
        }
  _ <-
    liftIO $ atomicModifyIORef' ref (const (updeatedResults, updeatedResults))
  return $ T.pack "{}"

editTodos :: ToodlesState -> EditTodoRequest -> Handler T.Text
editTodos (ToodlesState ref _) req = do
  (TodoListResult r _) <- liftIO $ readIORef ref
  let editedList =
        map
          (\t ->
             if willEditTodo req t
               then editTodo req t
               else t)
          r
      editedFilteredList = filter (willEditTodo req) editedList
  _ <- mapM_ recordUpdates editedFilteredList
  return $ T.pack "{}"
  where
    willEditTodo :: EditTodoRequest -> TodoEntry -> Bool
    willEditTodo editRequest entry = Main.id entry `elem` editIds editRequest

    editTodo :: EditTodoRequest -> TodoEntry -> TodoEntry
    editTodo editRequest entry =
      let newAssignee = if isJust (setAssignee editRequest) && (not . T.null . fromJust $ setAssignee editRequest)
            then setAssignee editRequest
            else assignee entry
          newPriority = if isJust (setPriority editRequest) then setPriority editRequest else priority entry in

        entry {assignee = newAssignee,
               tags = tags entry ++ addTags editRequest,
               priority = newPriority,
               customAttributes = nub $ customAttributes entry ++ addKeyVals editRequest}

    recordUpdates :: MonadIO m => TodoEntry -> m ()
    recordUpdates t = void $ updateTodoLinesInFile renderTodo t

renderTodo :: TodoEntry -> [T.Text]
renderTodo t =
  let comment =
        fromJust $ lookup ("." <> getExtension (sourceFile t)) fileTypeToComment
      detail =
        T.pack "TODO (" <>
        (T.pack $
         Data.String.Utils.join
           "|"
           (map T.unpack $ [fromMaybe "" $ assignee t] ++
            listIfNotNull (fmap (T.pack . maybe "" ((\n -> "p=" ++ n) . show)) priority t) ++
            tags t ++
            map (\a -> fst a <> "=" <> snd a) (customAttributes t))) <>
        (T.pack ") ")
      fullNoComments = mapHead (\l -> detail <> "- " <> l) $ body t
      commented = map (\l -> comment <> " " <> l) fullNoComments in
      mapHead (\l -> leadingText t <> l) $
        mapInit (\l -> foldl (<>) "" [" " | _ <- [1..(T.length $ leadingText t)]] <> l) commented

mapHead :: (a -> a) -> [a] -> [a]
mapHead f (x:xs) = f x : xs
mapHead _ xs     = xs

mapInit :: (a -> a) -> [a] -> [a]
mapInit f (x:xs) = [x] ++ map f xs
mapInit _ x      = x

listIfNotNull :: T.Text -> [T.Text]
listIfNotNull "" = []
listIfNotNull s  = [s]

root :: ToodlesState -> [T.Text] -> Handler BZ.Html
root (ToodlesState _ dPath) path =
  if null path then
    liftIO $ BZ.preEscapedToHtml <$> readFile (dPath ++ "/html/index.html")
  else throwError $ err404 { errBody = "Not found" }

app :: ToodlesState -> Application
app s = serve toodlesAPI $ server s

isEntryHead :: TodoEntry -> Bool
isEntryHead TodoEntryHead {} = True
isEntryHead _                = False

isBodyLine :: TodoEntry -> Bool
isBodyLine (TodoBodyLine _) = True
isBodyLine _                = False

combineTodo :: TodoEntry -> TodoEntry -> TodoEntry
combineTodo (TodoEntryHead i b a p n entryPriority attrs entryTags entryLeadingText) (TodoBodyLine l) =
  TodoEntryHead i (b ++ [l]) a p n entryPriority  attrs entryTags entryLeadingText
combineTodo _ _ = error "Can't combine todoEntry of these types"

data SourceFile = SourceFile
  { fullPath    :: FilePath
  , sourceLines :: [T.Text]
  } deriving (Show)

newtype AssigneeFilterRegex =
  AssigneeFilterRegex T.Text
  deriving (Show, Data, Eq)

newtype SearchFilter =
  AssigneeFilter AssigneeFilterRegex
  deriving (Show, Data, Eq)

data ToodlesArgs = ToodlesArgs
  { directory       :: FilePath
  , assignee_search :: Maybe SearchFilter
  , limit_results   :: Int
  , port            :: Maybe Int
  , no_server       :: Bool
  } deriving (Show, Data, Typeable, Eq)

argParser :: ToodlesArgs
argParser =
  ToodlesArgs
  { directory = def &= typFile &= help "Root directory of your project"
  , assignee_search = def &= help "Filter todo's by assignee"
  , limit_results = def &= help "Limit number of search results"
  , port = def &= help "Run server on port"
  , no_server = def &= help "Output matching todos to the command line and exit"
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
  , (".kt", "//")
  , (".kts", "//")
  , (".m", "//")
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

-- TODO(avi|p=3|#cleanup) - fix and type this better
parseDetails ::
     T.Text -> (Maybe T.Text, Maybe T.Text, [(T.Text, T.Text)], [T.Text])
parseDetails toParse =
  let dataTokens = T.splitOn "|" toParse
      assigneeTo =
        find
          (\t ->
             not (T.null t) &&
             not (T.isInfixOf "=" t) && not (T.isPrefixOf "#" t))
          dataTokens
      allDetails =
        map (\[a, b] -> (a, b)) $ filter (\t -> length t == 2) $
        map (T.splitOn "=") dataTokens
      priorityVal = snd <$> find (\t -> T.strip (fst t) == "p") allDetails
      filteredDetails = filter (\t -> T.strip (fst t) /= "p") allDetails
      entryTags = filter (T.isPrefixOf "#") dataTokens
  in (assigneeTo, priorityVal, filteredDetails, entryTags)

inParens :: Parser a -> Parser a
inParens = between (symbol "(") (symbol ")")

stringToMaybe :: T.Text -> Maybe T.Text
stringToMaybe t =
  if T.null t
    then Nothing
    else Just t

fst4 :: (a, b, c, d) -> a
fst4 (x, _, _, _) = x

snd4 :: (a, b, c, d) -> b
snd4 (_, x, _, _) = x

thd4 :: (a, b, c, d) -> c
thd4 (_, _, x, _) = x

fth4 :: (a, b, c, d) -> d
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
  entryLeadingText <- manyTill anyChar (prefixParserForFileType $ getExtension path)
  _ <- symbol "TODO"
  entryDetails <- optional $ try (inParens $ many (noneOf [')', '(']))
  let parsedDetails = parseDetails . T.pack <$> entryDetails
      entryPriority = (readMaybe . T.unpack) =<< (snd4 =<< parsedDetails)
      otherDetails = maybe [] thd4 parsedDetails
      entryTags = maybe [] fth4 parsedDetails
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
      entryPriority
      otherDetails
      entryTags
      (T.pack entryLeadingText)

parseTodo :: FilePath -> LineNumber -> Parser TodoEntry
parseTodo path lineNum =
  try (parseTodoEntryHead path lineNum) <|> parseComment (getExtension path)

getAllFiles :: ToodlesConfig -> FilePath -> IO [SourceFile]
getAllFiles config path =
  E.catch
    (do putStrLn $ printf "Running toodles for path: %s" path
        files <- recurseDir SystemFS path
        let validFiles = filter (isValidFile config) files
        -- TODO(avi|p=3|#cleanup) - make sure it's a file first
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

ignoreFile :: ToodlesConfig -> FilePath -> Bool
ignoreFile (ToodlesConfig ignoredPaths) file =
  let p = T.pack file
  in T.isInfixOf "node_modules" p || T.isSuffixOf "pb.go" p ||
     T.isSuffixOf "_pb2.py" p ||
     any (\r -> file =~ r :: Bool) ignoredPaths

getExtension :: FilePath -> T.Text
getExtension path = last $ T.splitOn "." (T.pack path)

isValidFile :: ToodlesConfig ->  FilePath -> Bool
isValidFile config f =
  fileHasValidExtension f && not (ignoreFile config f)

runTodoParser :: SourceFile -> [TodoEntry]
runTodoParser (SourceFile path ls) =
  let parsedTodoLines =
        map
          (\(lineNum, lineText) -> parseMaybe (parseTodo path lineNum) lineText)
          (zip [1 ..] ls)
      groupedTodos = foldl foldTodoHelper ([], False) parsedTodoLines
  in fst groupedTodos

-- fold fn to concatenate todos that a multiple, single line comments
foldTodoHelper :: ([TodoEntry], Bool) -> Maybe TodoEntry -> ([TodoEntry], Bool)
foldTodoHelper (todoEntries :: [TodoEntry], currentlyBuildingTodoLines :: Bool) maybeTodo
  -- We're not on a todo line, keep going
  | isNothing maybeTodo = (todoEntries, False)
  -- We see the start of a new todo
  | isEntryHead $ fromJust maybeTodo = (todoEntries ++ [fromJust maybeTodo], True)
  -- We a body line of a todo to concatenate to the current one
  | isBodyLine (fromJust maybeTodo) && currentlyBuildingTodoLines =
    (init todoEntries ++ [combineTodo (last todoEntries) (fromJust maybeTodo)], True)
  | otherwise = (todoEntries, False)

prettyFormat :: TodoEntry -> String
prettyFormat (TodoEntryHead _ l a p n entryPriority _ _ _) =
  printf
    "Assignee: %s\n%s%s:%d\n%s"
    (fromMaybe "None" a)
    (maybe "" (\x -> "Priority: " ++ show x ++ "\n") entryPriority)
    p
    n
    (unlines $ map T.unpack l)
prettyFormat (TodoBodyLine _) = error "Invalid type for prettyFormat"

filterSearch :: Maybe SearchFilter -> (TodoEntry -> Bool)
filterSearch Nothing = const True
filterSearch (Just (AssigneeFilter (AssigneeFilterRegex query))) =
  \entry -> fromMaybe "" (assignee entry) == query

limitSearch :: [TodoEntry] -> Int -> [TodoEntry]
limitSearch todoList limit =
  if limit == 0
    then todoList
    else take limit todoList

runFullSearch :: ToodlesArgs -> IO TodoListResult
runFullSearch userArgs =
  let projectRoot = directory userArgs
  in do
        configExists <- doesFileExist $ projectRoot ++ "/.toodles.yaml"
        config <- if configExists
          then Y.decodeEither' . B8SS.pack <$> readFile (projectRoot ++ "/.toodles.yaml")
          else return . Right $ ToodlesConfig []
        when (isLeft config)
          $ putStrLn $ "[WARNING] Invalid .toodles.yaml: " ++ show config
        allFiles <- getAllFiles (fromRight (ToodlesConfig []) config) projectRoot
        let parsedTodos = concatMap runTodoParser allFiles
            filteredTodos =
              filter (filterSearch (assignee_search userArgs)) parsedTodos
            resultList = limitSearch filteredTodos $ limit_results userArgs
            indexedResults =
              map (\(i, r) -> r {Main.id = i}) $ zip [1 ..] resultList
        return $ TodoListResult indexedResults ""

getFullSearchResults :: ToodlesState -> Bool -> IO TodoListResult
getFullSearchResults (ToodlesState ref _) recompute =
  if recompute
    then do
      putStrLn "refreshing todo's"
      userArgs <- cmdArgs argParser >>= setAbsolutePath
      sResults <- runFullSearch userArgs
      atomicModifyIORef' ref (const (sResults, sResults))
    else putStrLn "cached read" >> readIORef ref

showRawFile :: ToodlesState -> Integer -> Handler BZ.Html
showRawFile (ToodlesState ref _) entryId = do
  (TodoListResult r _) <- liftIO $ readIORef ref
  let entry = find (\t -> Main.id t == entryId) r
  liftIO $
    maybe
      (return "Not found")
      (\e -> addAnchors <$> readFile (sourceFile e))
      entry

addAnchors :: String -> BZ.Html
addAnchors s =
  let codeLines = zip [1 ..] $ lines s
  in BZ.preEscapedToHtml $
     (unlines $
      map
        (\(i, l) -> printf "<pre><a name=\"line-%s\">%s</a></pre>" (show i) l)
        codeLines)

setAbsolutePath :: ToodlesArgs -> IO ToodlesArgs
setAbsolutePath toodlesArgs =
  let pathOrDefault =
        if T.null . T.pack $ directory toodlesArgs
          then "."
          else directory toodlesArgs
  in do absolute <- normalise_path <$> absolute_path pathOrDefault
        return $ toodlesArgs {directory = absolute}

main :: IO ()
main = do
  userArgs <- cmdArgs argParser >>= setAbsolutePath
  sResults <- runFullSearch userArgs
  case userArgs of
    (ToodlesArgs _ _ _ _ True) -> mapM_ (putStrLn . prettyFormat) $ todos sResults
    _ -> do
          let webPort = fromMaybe 9001 $ port userArgs
          ref <- newIORef sResults
          dataDir <- (++ "/web") <$> getDataDir
          putStrLn $ "serving on " ++ show webPort
          run webPort $ app $ ToodlesState ref dataDir
