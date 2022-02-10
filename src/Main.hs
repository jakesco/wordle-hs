module Main where

import Control.Monad (forever, when)
import Data.Char (toLower, toUpper)
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
      [] -> concatMap (pad . toUpper) p
      (g : gs) -> concatMap (pad . toUpper) g

data ValidationError = NotAWord | NotFiveChars
  deriving (Eq, Show)

data CharacterStatus = InRightPlace | InWord | NotInWord
  deriving (Eq, Show)

pad :: Char -> String
pad c = [' ', c, ' ']

isWord :: WordList -> String -> Bool
isWord (WordList wl) s = s `elem` wl

isWord' :: String -> Bool
isWord' = isWord allWords

isFiveChars :: String -> Bool
isFiveChars = (== 5) . length

isGreen :: String -> String -> [Bool]
isGreen answer = zipWith (==) answer

isYellow :: String -> String -> [Bool]
isYellow answer = map (`elem` answer)

validateGuess :: String -> Either ValidationError String
validateGuess guess =
  case (isFiveChars guess, isWord' guess) of
    (False, _) -> Left NotFiveChars
    (_, False) -> Left NotAWord
    (_, _) -> Right guess

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
handleGuess (Puzzle p guesses) guess = do
  let puzzle = Puzzle p (guess : guesses)
  print puzzle
  return puzzle

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
  case validateGuess $ map toLower guess of
    (Left NotFiveChars) -> putStrLn "Your guess must be a 5 letter word."
    (Left NotAWord) -> putStrLn "Your guess is not in the word list."
    (Right guess) -> handleGuess puzzle guess >>= gameLoop

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord allWords
  let puzzle = mkPuzzle (map toUpper word)
  gameLoop puzzle
