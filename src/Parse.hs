{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parse where

import           Types

import           Data.List                  (find)
import           Data.Maybe                 (fromJust, fromMaybe, isJust,
                                             isNothing)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void                  (Void)


import           Debug.Trace
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Read

type Parser = Parsec Void Text

symbol :: Text -> Parser Text
symbol = L.symbol space

parseComment :: TodoParserState -> Text -> Parser TodoEntry
parseComment state fileExtension =
  if state == ParseStateMultiLineComment
    then do
      let closingParser = symbol $ getMultiClosingForFileType fileExtension
      lineWithClosing <- optional . try $ manyTill anyChar closingParser
      lineWithOutClosing <- optional $ many anyChar
      return $ TodoBodyLine (T.pack (fromMaybe (fromJust lineWithOutClosing) lineWithOutClosing)) False (isJust lineWithClosing)
    else do
      single <- optional . try $ manyTill anyChar (symbol $ getCommentForFileType fileExtension)
      multi <-
        if (isJust single)
        then return Nothing
        else do
          optional . try $ manyTill anyChar (symbol $ getMultiOpeningForFileType fileExtension)
      if (isJust single || isJust multi)
        then do
          b <- many anyChar
          return $ TodoBodyLine (T.pack b) (isJust  multi) (getMultiClosingForFileType fileExtension `T.isInfixOf` (T.pack b))
        else
          fail "No open comment marker found"

-- TODO(avi|#pr) refactor the 3 fn's here
getCommentForFileType :: Text -> Text
getCommentForFileType fileExtension =
    fromMaybe unkownMarker (singleCommentStart =<< (find (\a -> (extension a) == adjustedExtension) fileTypeToComment))
    where
    adjustedExtension =
        if T.isPrefixOf "." fileExtension
            then fileExtension
            else "." <> fileExtension

getMultiClosingForFileType :: Text -> Text
getMultiClosingForFileType fileExtension =
    fromMaybe unkownMarker (multiLineClose =<< (find (\a -> (extension a) == adjustedExtension) fileTypeToComment))
    where
    adjustedExtension =
        if T.isPrefixOf "." fileExtension
            then fileExtension
            else "." <> fileExtension

getMultiOpeningForFileType :: Text -> Text
getMultiOpeningForFileType fileExtension =
    fromMaybe unkownMarker (multiLineOpen =<< (find (\a -> (extension a) == adjustedExtension) fileTypeToComment))
    where
    adjustedExtension =
        if T.isPrefixOf "." fileExtension
            then fileExtension
            else "." <> fileExtension

integer :: Parser Integer
integer = lexeme $ L.signed space L.decimal

    where
    lexeme :: Parser a -> Parser a
    lexeme = L.lexeme space

parseAssignee :: Parser String
parseAssignee = many (noneOf [')', '|', '='])

-- TODO(avi|p=3|#cleanup) - fix and type this better
parseDetails :: Text -> (Maybe Text, Maybe Text, [(Text, Text)], [Text])
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

-- | parse "hard-coded" flags, and user-defined flags if any
parseFlag :: [UserFlag] -> Parser Flag
parseFlag us = foldr (\a b -> b <|> foo a) (try parseFlagHardcoded) us
  where
    foo :: UserFlag -> Parser Flag
    foo (UserFlag x) = try (symbol x *> pure (UF $ UserFlag x))

-- | parse flags TODO, FIXME, XXX
parseFlagHardcoded :: Parser Flag
parseFlagHardcoded =
      try (symbol "TODO"  *> pure TODO )
  <|> try (symbol "FIXME" *> pure FIXME)
  <|>     (symbol "XXX"   *> pure XXX  )

newtype MultineCommentEnclosing = MultineCommentEnclosing (Text, Text)

data FileTypeDetails = FileTypeDetails
  { extension          :: Text
  , singleCommentStart :: Maybe Text
  , multilineEnclosing :: Maybe MultineCommentEnclosing
  }

multiLineClose :: FileTypeDetails -> Maybe Text
multiLineClose (FileTypeDetails _ _ (Just (MultineCommentEnclosing (_, b)))) = Just b
multiLineClose _ = Nothing

multiLineOpen :: FileTypeDetails -> Maybe Text
multiLineOpen (FileTypeDetails _ _ (Just (MultineCommentEnclosing (a, _)))) = Just a
multiLineOpen _ = Nothing

fileTypeToComment :: [FileTypeDetails]
fileTypeToComment =
  [ FileTypeDetails ".c" (Just "//") Nothing
  , FileTypeDetails ".cc" (Just "//") Nothing
  , FileTypeDetails ".clj" (Just ";;") Nothing
  , FileTypeDetails ".cpp" (Just "//") Nothing
  , FileTypeDetails ".cxx" (Just "//") Nothing
  , FileTypeDetails ".c++" (Just "//") Nothing
  , FileTypeDetails ".cs" (Just "//") Nothing
  , FileTypeDetails ".ex" (Just "#") Nothing
  , FileTypeDetails ".erl" (Just "%") Nothing
  , FileTypeDetails ".go" (Just "//") Nothing
  , FileTypeDetails ".h" (Just "//") Nothing
  , FileTypeDetails ".hh" (Just "//") Nothing
  , FileTypeDetails ".hpp" (Just "//") Nothing
  , FileTypeDetails ".hs" (Just "--") (Just (MultineCommentEnclosing ("{-", "-}")))
  , FileTypeDetails ".hxx" (Just "//") Nothing
  , FileTypeDetails ".h++" (Just "//") Nothing
  , FileTypeDetails ".java" (Just "//") Nothing
  , FileTypeDetails ".js" (Just "//") Nothing
  , FileTypeDetails ".jsx" (Just "//") Nothing
  , FileTypeDetails ".kt" (Just "//") Nothing
  , FileTypeDetails ".kts" (Just "//") Nothing
  , FileTypeDetails ".lua" (Just "--") Nothing
  , FileTypeDetails ".m" (Just "//") Nothing
  , FileTypeDetails ".php" (Just "//") Nothing
  , FileTypeDetails ".proto" (Just "//") Nothing
  , FileTypeDetails ".py" (Just "#") Nothing
  , FileTypeDetails ".rb" (Just "#") Nothing
  , FileTypeDetails ".rs" (Just "//") Nothing
  , FileTypeDetails ".scala" (Just "//") Nothing
  , FileTypeDetails ".sh" (Just "#") Nothing
  , FileTypeDetails ".swift" (Just "///") Nothing
  , FileTypeDetails ".ts" (Just "//") Nothing
  , FileTypeDetails ".tsx" (Just "//") Nothing
  , FileTypeDetails ".txt" (Just "") Nothing
  , FileTypeDetails ".vue" (Just "//") Nothing
  , FileTypeDetails ".yaml" (Just "#") Nothing
  ]

singleLineCommentForExtension :: Text -> Maybe Text
singleLineCommentForExtension fileExtension =
  (find (\f -> extension f == fileExtension) fileTypeToComment) >>= singleCommentStart

multiLineOpenCommentForExtension :: Text -> Maybe Text
multiLineOpenCommentForExtension fileExtension =
  (find (\f -> extension f == fileExtension) fileTypeToComment) >>=
  multilineEnclosing >>=
  (\(MultineCommentEnclosing a) -> return $ fst a)

-- Higher order function returning our folder.
fileParseFoldFn ::
  -- partial fn params
     [UserFlag]
  -> FilePath
  -- returns a fold function of:
  -> (TodoParserState, [Maybe TodoEntry])
  -> (Integer, Text)
  -> (TodoParserState, [Maybe TodoEntry])
fileParseFoldFn userFlags sourceFile (currentLineState, pastList) (lineNumber, line) =
  let parsedLine = parseMaybe (parseTodo currentLineState userFlags sourceFile lineNumber) line
      newState = (nextState currentLineState parsedLine) in
  trace (show sourceFile ++ ":" ++ show lineNumber ++ "  " ++ (T.unpack line) ++ "  " ++ show parsedLine ++ " | " ++ show newState) (newState, pastList ++ [parsedLine])

-- FIXME(p=0) this seems broken
nextState :: TodoParserState -> Maybe TodoEntry -> TodoParserState
nextState _ Nothing                            = ParseStateUnknown
nextState _ (Just (TodoBodyLine _ _ True))     = ParseStateUnknown
nextState _ (Just (TodoBodyLine _ True False)) = ParseStateMultiLineComment
nextState s (Just (TodoBodyLine _ False False)) = s
nextState _ (Just (TodoBodyLine {})) = ParseStateSingleComment
nextState _ (Just (TodoEntryHead _ _ _ _ _ _ _ _ _ _ _ True False)) = ParseStateMultiLineComment
nextState _ (Just (TodoEntryHead _ _ _ _ _ _ _ _ _ _ MultiLine True False)) = ParseStateMultiLineComment
nextState _ (Just TodoEntryHead {}) = ParseStateUnknown
nextState a b = error ("No next state for " ++ show a ++ "   " ++ show b)

runTodoParser :: [UserFlag] -> SourceFile -> [TodoEntry]
runTodoParser us (SourceFile path ls) =
  let parsedTodoLines =
        foldl
          (fileParseFoldFn
          us
          path)
          (ParseStateUnknown, [])
          (zip [1 ..] ls)
      groupedTodos = foldl foldTodoHelper ([], False) (snd parsedTodoLines)
  in trace (path ++ " " ++ (show $ fst groupedTodos)) fst groupedTodos

  where
    -- fold fn to concatenate todos that a multiple, single line comments
    foldTodoHelper :: ([TodoEntry], Bool) -> Maybe TodoEntry -> ([TodoEntry], Bool)
    foldTodoHelper (todoEntries, currentlyBuildingTodoLines) maybeTodo
        -- We're not on a todo line, keep going
        | isNothing maybeTodo = (todoEntries, False)
        -- We see the start of a new todo
        | isEntryHead $ fromJust maybeTodo = (todoEntries ++ [fromJust maybeTodo], True)
        -- We a body line of a todo to concatenate to the current one
        | isBodyLine (fromJust maybeTodo) && currentlyBuildingTodoLines =
            (init todoEntries ++ [combineTodo (last todoEntries) (fromJust maybeTodo)], True)
        | otherwise = (todoEntries, False)

          where
            isEntryHead :: TodoEntry -> Bool
            isEntryHead TodoEntryHead {} = True
            isEntryHead _                = False

            isBodyLine :: TodoEntry -> Bool
            isBodyLine TodoBodyLine {} = True
            isBodyLine _               = False

            combineTodo :: TodoEntry -> TodoEntry -> TodoEntry
            combineTodo (TodoEntryHead i b a p n entryPriority f attrs entryTags entryLeadingText t isOpened _) (TodoBodyLine l _ isClosed) =
                TodoEntryHead i (b ++ [l]) a p n entryPriority f attrs entryTags entryLeadingText t isOpened isClosed
            combineTodo  (TodoBodyLine l isOpened _) (TodoEntryHead i b a p n entryPriority f attrs entryTags entryLeadingText t _ isClosed) =
                TodoEntryHead i ([l] ++ b) a p n entryPriority f attrs entryTags entryLeadingText t isOpened isClosed
            combineTodo (TodoBodyLine l isOpened _) (TodoBodyLine r _ isClosed) =
              TodoBodyLineCombined (l : [r]) isOpened isClosed
            combineTodo (TodoBodyLineCombined l isOpened _) (TodoBodyLine r _ isClosed) =
              TodoBodyLineCombined (l ++ [r]) isOpened isClosed
            combineTodo (TodoBodyLineCombined l isOpened _) (TodoEntryHead i b a p n entryPriority f attrs entryTags entryLeadingText t _ isClosed) =
                TodoEntryHead i (l ++ b) a p n entryPriority f attrs entryTags entryLeadingText t isOpened isClosed
            combineTodo _ _ = error "Can't combine todoEntry of these types"

getExtension :: FilePath -> Text
getExtension path = last $ T.splitOn "." (T.pack path)

stringToMaybe :: Text -> Maybe Text
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

unkownMarker :: Text
unkownMarker = "UNKNOWN-DELIMETER-UNKNOWN-DELIMETER-UNKNOWN-DELIMETER"

data TodoParserState
  = ParseStateUnknown
  | ParseStateSource
  | ParseStateSingleComment
  | ParseStateMultiLineComment deriving (Eq, Show)

data LeadingTextKind = SingleLT | MultiLT | NonOpenedComment deriving (Eq, Show)

takeShorter singleLeadingText multiLeadingText
  | length singleLeadingText == length multiLeadingText = ""
  | null singleLeadingText                              = multiLeadingText
  | null multiLeadingText                               = singleLeadingText
  | length singleLeadingText > length multiLeadingText  = multiLeadingText
  | length singleLeadingText < length multiLeadingText  = singleLeadingText

parseTodo :: TodoParserState -> [UserFlag] -> FilePath -> LineNumber -> Parser TodoEntry
parseTodo state us path lineNum = try (parseTodoEntryHead us)
                            <|> parseComment state (getExtension path)
  where
    parseTodoEntryHead :: [UserFlag] -> Parser TodoEntry
    parseTodoEntryHead uf =
      if state == ParseStateMultiLineComment
        then do
          leadingTextMulti <- optional (try $ many spaceChar)
          parseEntryHead NonOpenedComment (fromMaybe "" leadingTextMulti)
        else do
          entryLeadingTextSingle <- optional (try (manyTill anyChar (lookAhead . prefixParserForFileType $ getExtension path)))
          entryLeadingTextMulti <-
            if isNothing entryLeadingTextSingle
            then
              optional (manyTill anyChar (lookAhead . multiPrefixParserForFileType $ getExtension path))
            else
              return Nothing

          commentOpenSingle <- optional . try $ prefixParserForFileType $ getExtension path
          commentOpenMulti <- optional . try $ multiPrefixParserForFileType $ getExtension path
          let leadingTextKind = case (commentOpenSingle, commentOpenMulti) of
                (Just _, Nothing)  -> SingleLT
                (Nothing, Just _)  -> MultiLT
                (Nothing, Nothing) -> NonOpenedComment

  -- select the shorter leading text, and update leadingTextKind enum accordingly
              leadingText = trace ("takeShorter: " ++ show entryLeadingTextSingle ++ " | " ++ show entryLeadingTextMulti) takeShorter ( (fromMaybe "" entryLeadingTextSingle)) ( (fromMaybe "" entryLeadingTextMulti))

          parseEntryHead leadingTextKind leadingText

          where

          parseEntryHead leadingTextKind leadingText = do
            f <- parseFlag uf
            entryDetails <- optional $ try (inParens $ many (noneOf [')', '(']))
            let parsedDetails = parseDetails . T.pack <$> entryDetails
                entryPriority = (readMaybe . T.unpack) =<< (snd4 =<< parsedDetails)
                otherDetails = maybe [] thd4 parsedDetails
                entryTags = maybe [] fth4 parsedDetails
            _ <- optional $ symbol "-"
            _ <- optional $ symbol ":"
            let closingParser = symbol $ getMultiClosingForFileType (getExtension path)
            lineWithClosing <- optional . try $ manyTill anyChar closingParser
            lineWithOutClosing <- optional $ many anyChar
            return $
                TodoEntryHead
                0
                [T.pack (fromMaybe (fromJust lineWithOutClosing) lineWithClosing)]
                (stringToMaybe . T.strip $ fromMaybe "" (fst4 =<< parsedDetails))
                path
                lineNum
                entryPriority
                f
                otherDetails
                entryTags
                (T.pack leadingText)
                (if leadingTextKind == SingleLT then SingleLine else MultiLine)
                (leadingTextKind == MultiLT)
                (isJust lineWithClosing)

          inParens :: Parser a -> Parser a
          inParens = between (symbol "(") (symbol ")")

          prefixParserForFileType :: Text -> Parser Text
          prefixParserForFileType fileExtension = symbol . getCommentForFileType $ fileExtension

          multiPrefixParserForFileType :: Text -> Parser Text
          multiPrefixParserForFileType fileExtension = symbol . getMultiOpeningForFileType $ fileExtension
