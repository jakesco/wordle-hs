module Main where

import Control.Monad (forever, when)
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

data Puzzle = Puzzle String [String]

instance Show Puzzle where
  show (Puzzle p guesses) =
    case guesses of
      [] -> show $ concatMap (pad . toUpper) p
      (g : gs) -> show $ concatMap (pad . toUpper) g

pad :: Char -> String
pad c = [' ', c, ' ']

guessesMade :: Puzzle -> Int
guessesMade (Puzzle _ guesses) = length guesses + 1

maxGuesses :: Int
maxGuesses = 6

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

mkPuzzle :: String -> Puzzle
mkPuzzle s = Puzzle s []

handleGuess :: Puzzle -> String -> IO Puzzle
handleGuess (Puzzle p guesses) guess = return $ Puzzle p (guess : guesses)

gameOver :: Puzzle -> IO ()
gameOver puzzle@(Puzzle word guesses) =
  when (length guesses >= maxGuesses) $
    do
      putStrLn $ "Game Over! The word was " ++ word ++ "."
      exitSuccess

gameWin :: Puzzle -> IO ()
gameWin (Puzzle word _) =
  do
    putStrLn $ "You Win! The word was " ++ word ++ "."
    exitSuccess

gameLoop :: Puzzle -> IO ()
gameLoop puzzle = forever $ do
  gameOver puzzle
  --gameWin puzzle
  putStrLn $ "Enter your guess [" ++ show (guessesMade puzzle) ++ "/" ++ show maxGuesses ++ "]"
  putStr "> "
  guess <- getLine
  if length guess == 5
    then handleGuess puzzle guess >>= gameLoop
    else putStrLn "Your guess must be a 5 letter word."

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord allWords
  let puzzle = mkPuzzle (map toUpper word)
  gameLoop puzzle
