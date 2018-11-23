{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Parse
import           Types

import           Data.Either

import qualified Data.Text       as T
import           Test.Hspec
import           Text.Megaparsec

bodyText (TodoBodyLine a _ _) = a
bodyText t                    = T.unlines (body t)

hasClosing (TodoBodyLine _ _ a) = a
hasClosing t                    = entryHeadClosed t

hasOpening (TodoBodyLine _ t _) = t
hasOpening t                    = entryHeadOpened t

defaultBodyLine = (TodoBodyLine "default bad" False False)

main :: IO ()
main = hspec $
  describe "Toodles" $ do

  it "parses comments (non todo entry head's)" $ do

    let haskellSingleComment = " -- a plain comment"
        haskellMultiComment  = "a plain comment"
        haskellMultiClose= "-}"
        singleHaskellParser  = parse (parseComment ParseStateSingleComment ".hs") ""
        multiHaskellParser   = parse (parseComment ParseStateMultiLineComment ".hs") ""

    let singleParsed = singleHaskellParser haskellSingleComment
    singleParsed `shouldSatisfy` isRight
    bodyText (fromRight defaultBodyLine singleParsed) `shouldSatisfy` (\s -> s `T.isInfixOf` "a plain comment")

    let multiParsed = multiHaskellParser haskellMultiComment
    multiParsed `shouldSatisfy` isRight
    let result = fromRight defaultBodyLine multiParsed
    hasClosing result `shouldBe` False

    let multiParsedWithClosing = multiHaskellParser (haskellMultiComment <> " -} ")
    multiParsedWithClosing `shouldSatisfy` isRight
    let result2 = fromRight defaultBodyLine multiParsedWithClosing
    putStrLn . T.unpack $ bodyText result2
    bodyText result2 `shouldSatisfy` (\s -> s `T.isInfixOf` "a plain comment")
    hasClosing result2 `shouldBe` True
    return ()

    let multiCloseParsed = multiHaskellParser haskellMultiClose
    multiCloseParsed `shouldSatisfy` isRight
    let result3 = fromRight defaultBodyLine multiCloseParsed
    hasClosing result3 `shouldBe` True
    hasOpening result3 `shouldBe` False

  it "parses TODOs" $ do

    let haskellSingleTodo = " -- TODO(avi|p=1|#tag|key=val) some stuff we need to fix"
        haskellEnclosedTodo = " {- TODO(avi|p=1|#tag|key=val) some stuff we need to fix -} "
        haskellMultiOpenTodo = " {- TODO(avi|p=1|#tag|key=val) some stuff we need to fix"
        haskellMultiCloseTodo = " TODO(avi|p=1|#tag|key=val) some stuff we need to fix -}"
        haskellBareMultiTodo = "      TODO(avi|p=1|#tag|key=val) some stuff we need to fix"
        emptyMultiOpen = " {- "
        emptyMulti = ""
        unknownStateParser = parse (parseTodo ParseStateUnknown [] "hello.hs" 10 ) ""
        multiLineStateParser = parse (parseTodo ParseStateMultiLineComment [] "hello.hs" 10 ) ""

    let singleParsed = unknownStateParser haskellSingleTodo
    singleParsed `shouldSatisfy` isRight
    bodyText (fromRight defaultBodyLine singleParsed) `shouldBe` "some stuff we need to fix\n"

    let multiParsed = unknownStateParser haskellEnclosedTodo
    multiParsed `shouldSatisfy` isRight
    let result2 = fromRight defaultBodyLine multiParsed
    (bodyText result2) `shouldBe` "some stuff we need to fix \n"
    (hasClosing result2) `shouldBe` True
    (hasOpening result2) `shouldBe` True

    let multiOpenParsed = unknownStateParser haskellMultiOpenTodo
    multiOpenParsed `shouldSatisfy` isRight
    let result3 = fromRight defaultBodyLine multiOpenParsed
    bodyText result3 `shouldBe` "some stuff we need to fix\n"
    hasClosing result3 `shouldBe` False
    hasOpening result3 `shouldBe` True

    unknownStateParser haskellMultiCloseTodo `shouldSatisfy` isLeft

    let mutliCloseParsed = multiLineStateParser haskellMultiCloseTodo
    mutliCloseParsed `shouldSatisfy` isRight
    let result4 = fromRight defaultBodyLine mutliCloseParsed
    bodyText result4 `shouldBe` "some stuff we need to fix \n"
    hasClosing result4 `shouldBe` True
    hasOpening result4 `shouldBe` False

    let emptyOpen = unknownStateParser emptyMultiOpen
    emptyOpen `shouldSatisfy` isRight
    let result5 = fromRight defaultBodyLine emptyOpen
    hasOpening result5 `shouldBe` True
    hasClosing result5 `shouldBe` False

    let emptyMultiParsed = multiLineStateParser emptyMulti
    emptyMultiParsed `shouldSatisfy` isRight
    let result6 = fromRight defaultBodyLine emptyMultiParsed
    hasOpening result6 `shouldBe` False
    hasClosing result6 `shouldBe` False

    let bareMultiParsed = multiLineStateParser haskellBareMultiTodo
    bareMultiParsed `shouldSatisfy` isRight
    let result7 = fromRight defaultBodyLine bareMultiParsed
    hasOpening result7 `shouldBe` False
    hasClosing result7 `shouldBe` False
    bodyText result7 `shouldBe` "some stuff we need to fix\n"

    return ()
