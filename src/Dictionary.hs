{-# LANGUAGE TemplateHaskell #-}

module Dictionary where

import Prelude hiding (filter)
import Data.ByteString (ByteString, split, filter)
import Data.ByteString.Char8 (unpack)
import Data.FileEmbed (embedFile)

newtype WordList = WordList [String]
  deriving (Eq, Show)

puzzleInput :: ByteString
puzzleInput = $(embedFile "words/puzzle.txt")

puzzleWords :: WordList
puzzleWords = WordList $ map unpack $ split 10 $ filter (/=13) puzzleInput

allWordsInput :: ByteString
allWordsInput = $(embedFile "words/all.txt")

allWords :: WordList
allWords = WordList $ map unpack $ split 10 $ filter (/=13) allWordsInput
