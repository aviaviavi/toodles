{-# LANGUAGE OverloadedStrings,
             ScopedTypeVariables #-}

module Parse where

import           Types

import           Data.List                       (find)
import           Data.Maybe                      (fromJust, fromMaybe, isNothing)
import           Data.Text                       (Text)
import qualified Data.Text                  as T
import           Data.Void                       (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L 
import           Text.Read

type Parser = Parsec Void Text

symbol :: Text -> Parser Text
symbol = L.symbol space

parseComment :: Text -> Parser TodoEntry
parseComment extension = do
    _ <- manyTill anyChar (symbol $ getCommentForFileType extension)
    b <- many anyChar
    return . TodoBodyLine $ T.pack b

getCommentForFileType :: Text -> Text
getCommentForFileType extension =
    fromMaybe unkownMarker $ lookup adjustedExtension fileTypeToComment
    where
    adjustedExtension =
        if T.isPrefixOf "." extension
            then extension
            else "." <> extension

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

fileTypeToComment :: [(Text, Text)]
fileTypeToComment =
  [ (".c", "//")
  , (".cc", "//")
  , (".clj", ";;")
  , (".cpp", "//")
  , (".cxx", "//")
  , (".c++", "//")
  , (".cs", "//")  
  , (".ex", "#")
  , (".erl", "%")
  , (".go", "//")
  , (".h", "//")
  , (".hh", "//")
  , (".hpp", "//")
  , (".hs", "--")
  , (".hxx", "//")
  , (".h++", "//")
  , (".java", "//")
  , (".js", "//")
  , (".kt", "//")
  , (".kts", "//")
  , (".lua", "--")
  , (".m", "//")
  , (".php", "//")
  , (".proto", "//")
  , (".py", "#")
  , (".rb", "#")
  , (".rs", "//")
  , (".scala", "//")
  , (".sh", "#")
  , (".swift", "///")
  , (".ts", "//")
  , (".tsx", "//")
  , (".txt", "")
  , (".yaml", "#")
  ]

runTodoParser :: SourceFile -> [TodoEntry]
runTodoParser (SourceFile path ls) =
  let parsedTodoLines =
        map
          (\(lineNum, lineText) -> parseMaybe (parseTodo path lineNum) lineText)
          (zip [1 ..] ls)
      groupedTodos = foldl foldTodoHelper ([], False) parsedTodoLines
  in fst groupedTodos

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
            isBodyLine (TodoBodyLine _) = True
            isBodyLine _                = False

            combineTodo :: TodoEntry -> TodoEntry -> TodoEntry
            combineTodo (TodoEntryHead i b a p n entryPriority attrs entryTags entryLeadingText) (TodoBodyLine l) =
                TodoEntryHead i (b ++ [l]) a p n entryPriority  attrs entryTags entryLeadingText
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

parseTodo :: FilePath -> LineNumber -> Parser TodoEntry
parseTodo path lineNum = try parseTodoEntryHead
                     <|> parseComment (getExtension path)

    where
    parseTodoEntryHead :: Parser TodoEntry
    parseTodoEntryHead = do
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

        where
        inParens :: Parser a -> Parser a
        inParens = between (symbol "(") (symbol ")")

        prefixParserForFileType :: Text -> Parser Text
        prefixParserForFileType "org" = try (symbol "****")
                                    <|> try (symbol "***")
                                    <|> try (symbol "**")
                                    <|> try (symbol "*")
                                    <|> symbol "-"
        prefixParserForFileType extension = symbol . getCommentForFileType $ extension
