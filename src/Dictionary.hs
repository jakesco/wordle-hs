{-# LANGUAGE TemplateHaskell #-}

module Dictionary where

import Data.ByteString (ByteString, filter, split)
import Data.ByteString.Char8 (unpack)
import Data.FileEmbed (embedFile)
import Prelude hiding (filter)

newtype WordList = WordList [String]
  deriving (Eq, Show)

alphabet :: String
alphabet = "ABCDEFGHIJKLM\nNOPQRSTUVWXYZ"

puzzleInput :: ByteString
puzzleInput = $(embedFile "words/puzzle.txt")

puzzleWords :: WordList
puzzleWords = WordList $ map unpack $ split 10 $ filter (/= 13) puzzleInput

allWordsInput :: ByteString
allWordsInput = $(embedFile "words/all.txt")

allWords :: WordList
allWords = WordList $ map unpack $ split 10 $ filter (/= 13) allWordsInput
