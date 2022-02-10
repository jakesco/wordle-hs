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
import System.Console.Pretty (Color (..), bgColor)
import Words (WordList (..), allWords)

data Puzzle = Puzzle String [Guess]

instance Show Puzzle where
  show (Puzzle p guesses) =
    case guesses of
      [] -> concatMap (pad . toUpper) p
      ((Guess g) : _) -> presentGuess g

data ValidationError = NotAWord | NotFiveChars
  deriving (Eq, Show)

data CharStatus = InRightPlace | InWord | NotInWord
  deriving (Eq, Show)

newtype Guess = Guess [(Char, CharStatus)]
  deriving (Eq, Show)

maxGuesses :: Int
maxGuesses = 6

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

mkPuzzle :: String -> Puzzle
mkPuzzle s = Puzzle s []

presentGuess :: [(Char, CharStatus)] -> String
presentGuess [] = []
presentGuess (g : gs) =
  (getColor (snd g) $ pad (fst g)) ++ (presentGuess gs)

pad :: Char -> String
pad c = [' ', toUpper c, ' ']

getColor cs =
  case cs of
    InRightPlace -> bgColor Green
    InWord       -> bgColor Yellow
    NotInWord    -> bgColor Red

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

guessesMade :: Puzzle -> Int
guessesMade (Puzzle _ guesses) = length guesses + 1

validateGuess :: String -> Either ValidationError String
validateGuess guess =
  case (isFiveChars guess, isWord' guess) of
    (False, _) -> Left NotFiveChars
    (_, False) -> Left NotAWord
    (_, _) -> Right guess

checkGuess :: String -> String -> [CharStatus]
checkGuess answer guess =
  let check = zip (isGreen answer guess) (isYellow answer guess) in
    map (\(g, y) ->
           case (g, y) of
             (True, _) -> InRightPlace
             (_, True) -> InWord
             (_, _)    -> NotInWord) check

handleGuess :: Puzzle -> String -> IO Puzzle
handleGuess (Puzzle p guesses) guess = do
  let puzzle =
        Puzzle p (Guess (zip guess (checkGuess p guess)) : guesses)
  print puzzle
  return puzzle

gameOver :: Puzzle -> IO ()
gameOver puzzle@(Puzzle word guesses) =
  when (length guesses >= maxGuesses) $
    do
      putStrLn $ "Game Over! The word was " ++ word ++ "."
      exitSuccess

gameWin :: Puzzle -> IO ()
gameWin (Puzzle word guesses) =
  case guesses of
    [] -> return ()
    (Guess g):_ ->
      when (all ((==InRightPlace) . snd) g) $
      do
        putStrLn $ "You Win! The word was " ++ word ++ "."
        exitSuccess

gameLoop :: Puzzle -> IO ()
gameLoop puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Enter your guess ["
    ++ show (guessesMade puzzle)
    ++ "/" ++ show maxGuesses ++ "]"
  putStr "> "
  guess <- getLine
  case validateGuess $ map toLower guess of
    (Left NotFiveChars) -> putStrLn "Your guess must be a 5 letter word."
    (Left NotAWord) -> putStrLn "Your guess is not in the word list."
    (Right guess) -> handleGuess puzzle (map toUpper guess) >>= gameLoop

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord allWords
  let puzzle = mkPuzzle (map toUpper word)
  gameLoop puzzle
