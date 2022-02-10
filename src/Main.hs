module Main where

import Control.Monad (forever, when)
import Data.Char (toLower, toUpper)
import Data.Maybe (isJust)
import Dictionary (WordList (..), allWords)
import Puzzle (Puzzle (..), checkGuess, guessWord, guessesMade, newPuzzle, puzzleWin, testPuzzle)
import System.Exit (exitSuccess)
import System.IO
  ( BufferMode (NoBuffering),
    hSetBuffering,
    stdout,
  )

data ValidationError = NotAWord | NotFiveChars
  deriving (Eq, Show)

maxGuesses :: Int
maxGuesses = 6

isWord :: WordList -> String -> Bool
isWord (WordList wl) s = s `elem` wl

isWord' :: String -> Bool
isWord' = isWord allWords

isFiveChars :: String -> Bool
isFiveChars = (== 5) . length

validateGuess :: String -> Either ValidationError String
validateGuess guess =
  case (isFiveChars guess, isWord' guess) of
    (False, _) -> Left NotFiveChars
    (_, False) -> Left NotAWord
    (_, _) -> Right guess

handleGuess :: Puzzle -> String -> IO Puzzle
handleGuess puzzle guess = do
  let newPuzzle = guessWord puzzle (map toUpper guess)
  print newPuzzle
  return newPuzzle

gameOver :: Puzzle -> IO ()
gameOver puzzle =
  when (guessesMade puzzle > maxGuesses) $
    do
      putStrLn $ "Game Over! The word was " ++ answer puzzle ++ "."
      exitSuccess

gameWin :: Puzzle -> IO ()
gameWin puzzle =
  when (puzzleWin puzzle) $
    do
      putStrLn $ "You Win! The word was " ++ answer puzzle ++ "."
      exitSuccess

gameLoop :: Puzzle -> IO ()
gameLoop puzzle = forever $ do
  gameWin puzzle
  gameOver puzzle
  putStrLn $
    "Enter your guess ["
      ++ show (guessesMade puzzle)
      ++ "/"
      ++ show maxGuesses
      ++ "]"
  putStr "> "
  guess <- getLine
  case validateGuess $ map toLower guess of
    (Left NotFiveChars) -> putStrLn "Your guess must be a 5 letter word."
    (Left NotAWord) -> putStrLn "Your guess is not in the word list."
    (Right guess) -> handleGuess puzzle guess >>= gameLoop

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  puzzle <- newPuzzle
  gameLoop puzzle
