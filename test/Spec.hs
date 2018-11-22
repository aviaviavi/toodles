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

defaultBodyLine = TodoBodyLine "default bad" False False

main :: IO ()
main = hspec $
  describe "Toodles" $ do
    let unknownStateParser = parse (parseTodo ParseStateUnknown [] "hello.hs" 10 ) ""
        multiLineStateParser = parse (parseTodo ParseStateMultiLineComment [] "hello.hs" 10 ) ""
        singleHaskellParser  = parse (parseComment ParseStateSingleComment ".hs") ""
        multiHaskellParser   = parse (parseComment ParseStateMultiLineComment ".hs") ""

    it "parses single line comments" $ do
      let haskellSingleComment = " -- a plain comment"
          singleParsed = singleHaskellParser haskellSingleComment
      singleParsed `shouldSatisfy` isRight
      bodyText (fromRight defaultBodyLine singleParsed) `shouldSatisfy` (`T.isInfixOf` "a plain comment")

    it "parses lines of multiline comments" $ do
      let haskellMultiComment  = "a plain comment"
          multiParsed = multiHaskellParser haskellMultiComment
      multiParsed `shouldSatisfy` isRight
      let result = fromRight defaultBodyLine multiParsed
      hasClosing result `shouldBe` False

      let multiParsedWithClosing = multiHaskellParser (haskellMultiComment <> " -} ")
      multiParsedWithClosing `shouldSatisfy` isRight
      let result2 = fromRight defaultBodyLine multiParsedWithClosing
      bodyText result2 `shouldSatisfy` ((`T.isInfixOf` "a plain comment"))
      hasClosing result2 `shouldBe` True

    it "parses multiline comment closings" $ do
      let haskellMultiClose = "-}"
      let multiCloseParsed = multiHaskellParser haskellMultiClose
      multiCloseParsed `shouldSatisfy` isRight
      let result3 = fromRight defaultBodyLine multiCloseParsed
      hasClosing result3 `shouldBe` True
      hasOpening result3 `shouldBe` False

    it "parses multiline comment opens" $ do
      let emptyMultiOpen = " {- "
          emptyOpen = unknownStateParser emptyMultiOpen
      emptyOpen `shouldSatisfy` isRight
      let result5 = fromRight defaultBodyLine emptyOpen
      hasOpening result5 `shouldBe` True
      hasClosing result5 `shouldBe` False

    it "parses empty lines as multiline comments " $ do
      let emptyMulti = ""
          emptyMultiParsed = multiLineStateParser emptyMulti
      emptyMultiParsed `shouldSatisfy` isRight
      let result6 = fromRight defaultBodyLine emptyMultiParsed
      hasOpening result6 `shouldBe` False
      hasClosing result6 `shouldBe` False

    it "parses single TODOs" $ do
      let haskellSingleTodo = " -- TODO(avi|p=1|#tag|key=val) some stuff we need to fix"
          singleParsed = unknownStateParser haskellSingleTodo
      singleParsed `shouldSatisfy` isRight
      bodyText (fromRight defaultBodyLine singleParsed) `shouldBe` "some stuff we need to fix\n"

    it "parses a multiline TODO fully enclosed on one line" $ do
      let haskellEnclosedTodo = " {- TODO(avi|p=1|#tag|key=val) some stuff we need to fix -} "
          multiParsed = unknownStateParser haskellEnclosedTodo
      multiParsed `shouldSatisfy` isRight
      let result2 = fromRight defaultBodyLine multiParsed
      bodyText result2 `shouldBe` "some stuff we need to fix \n"
      hasClosing result2 `shouldBe` True
      hasOpening result2 `shouldBe` True

    it "parses mutline TODO's on an opening line" $ do
      let haskellMultiOpenTodo = " {- TODO(avi|p=1|#tag|key=val) some stuff we need to fix"
          multiOpenParsed = unknownStateParser haskellMultiOpenTodo
      multiOpenParsed `shouldSatisfy` isRight
      let result3 = fromRight defaultBodyLine multiOpenParsed
      bodyText result3 `shouldBe` "some stuff we need to fix\n"
      hasClosing result3 `shouldBe` False
      hasOpening result3 `shouldBe` True

    it "parses mutline TODO's on a closing line" $ do
      let haskellMultiCloseTodo = " TODO(avi|p=1|#tag|key=val) some stuff we need to fix -}"
      unknownStateParser haskellMultiCloseTodo `shouldSatisfy` isLeft

      let mutliCloseParsed = multiLineStateParser haskellMultiCloseTodo
      mutliCloseParsed `shouldSatisfy` isRight
      let result4 = fromRight defaultBodyLine mutliCloseParsed
      bodyText result4 `shouldBe` "some stuff we need to fix \n"
      hasClosing result4 `shouldBe` True
      hasOpening result4 `shouldBe` False

    it "parses bare TODO's in the middle of an opening line" $ do
      let haskellBareMultiTodo = "      TODO(avi|p=1|#tag|key=val) some stuff we need to fix"
          bareMultiParsed = multiLineStateParser haskellBareMultiTodo
      bareMultiParsed `shouldSatisfy` isRight
      let result7 = fromRight defaultBodyLine bareMultiParsed
      hasOpening result7 `shouldBe` False
      hasClosing result7 `shouldBe` False
      bodyText result7 `shouldBe` "some stuff we need to fix\n"

