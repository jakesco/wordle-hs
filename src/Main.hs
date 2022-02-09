module Main where

import Control.Monad (forever)
import Data.Char (toUpper)
import Data.Maybe (isJust)
import System.Exit (exitSuccess)
import System.IO
  ( BufferMode (NoBuffering),
    hSetBuffering,
    stdout,
  )
import System.Random (randomRIO)
import Words (WordList (..), allWords)

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord allWords
  putStrLn word
