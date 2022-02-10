{-# LANGUAGE TemplateHaskell #-}

module Dictionary where

import Data.ByteString (ByteString, split)
import Data.ByteString.Char8 (unpack)
import Data.FileEmbed (embedFile)

newtype WordList = WordList [String]
  deriving (Eq, Show)

puzzleInput :: ByteString
puzzleInput = $(embedFile "words/puzzle.txt")

allWordsInput :: ByteString
allWordsInput = $(embedFile "words/all.txt")

puzzleWords :: WordList
puzzleWords = WordList $ map unpack $ split 10 puzzleInput

allWords :: WordList
allWords = WordList $ map unpack $ split 10 allWordsInput