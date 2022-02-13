module Wordle where

import Control.Monad (forever, when)
import Data.Char (toLower, toUpper)
import Dictionary (WordList (..), allWords)
import Puzzle
  ( Puzzle (..),
    checkGuess,
    guessWord,
    guessesMade,
    newPuzzle,
    puzzleWin,
    showGuessed,
  )
import System.Console.Pretty
  ( Color (..),
    Style (..),
    bgColor,
    color,
    style,
  )
import System.Exit (exitSuccess)

data ValidationError = NotAWord | NotFiveChars
  deriving (Eq, Show)

maxGuesses :: Int
maxGuesses = 6

introString :: String
introString =
  "Welcome to Wordle.hs!\nType "
    ++ color Green ":?"
    ++ " for help, "
    ++ color Green ":g"
    ++ " to view guessed letters, or "
    ++ color Green ":q"
    ++ " to quit."

helpString :: String
helpString =
  style Bold "Rules\n\n"
    ++ "You have 6 tries to guess the word. Each\n"
    ++ "guess must be a valid 5 letter word.\n\n"
    ++ "After each guess, the color of the letters\n"
    ++ "will indicate the accuracy of your guess.\n\n"
    ++ style Bold "Examples\n\n"
    ++ bgColor Green " S "
    ++ " L  A  T  E \nThe letter"
    ++ color Green " S "
    ++ "is in the word and in the correct spot.\n\n"
    ++ " J "
    ++ bgColor Yellow " I "
    ++ " F  F  Y \nThe letter"
    ++ color Yellow " I "
    ++ "is in the word but in the wrong spot.\n\n"
    ++ " P  L  U "
    ++ (style Bold . color Black . bgColor White) " C "
    ++ " K \nThe letter C is not in the word.\n"

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

runCommand :: String -> Puzzle -> IO ()
runCommand command puzzle =
  case command of
    ('h' : _) -> putStrLn helpString
    ('?' : _) -> putStrLn helpString
    ('g' : _) -> putStrLn $ showGuessed puzzle
    ('q' : _) -> do
      putStrLn $ "Exiting. The word was " ++ answer puzzle ++ "."
      exitSuccess
    _ -> putStrLn (command ++ " is not a valid command.")

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
  input <- getLine
  case input of
    ':' : rest -> runCommand rest puzzle
    _ ->
      case validateGuess $ map toLower input of
        (Left NotFiveChars) ->
          putStrLn "Your guess must be a 5 letter word."
        (Left NotAWord) ->
          putStrLn "Your guess is not in the word list."
        (Right g) -> handleGuess puzzle g >>= gameLoop
