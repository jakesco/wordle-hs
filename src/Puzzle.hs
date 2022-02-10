module Puzzle where

import Data.Char (toUpper)
import Dictionary (WordList (..), puzzleWords)
import System.Console.Pretty (Color (..), Pretty, bgColor)
import System.Random (randomRIO)

data Puzzle = Puzzle
  { answer :: String,
    guesses :: [Guess]
  }
  deriving (Eq)

instance Show Puzzle where
  show (Puzzle p guesses) =
    case guesses of
      [] -> concatMap (pad . toUpper) p
      ((Guess g) : _) -> presentGuess g

data CharStatus = InRightPlace | InWord | NotInWord
  deriving (Eq, Show)

newtype Guess = Guess [(Char, CharStatus)]
  deriving (Eq, Show)

presentGuess :: [(Char, CharStatus)] -> String
presentGuess = foldr (\g -> (++) (getColor (snd g) $ pad (fst g))) []

pad :: Char -> String
pad c = [' ', toUpper c, ' ']

getColor :: Pretty a => CharStatus -> a -> a
getColor cs =
  case cs of
    InRightPlace -> bgColor Green
    InWord -> bgColor Yellow
    NotInWord -> bgColor Red

isGreen :: String -> String -> [Bool]
isGreen answer = zipWith (==) answer

isYellow :: String -> String -> [Bool]
isYellow answer = map (`elem` answer)

guessesMade :: Puzzle -> Int
guessesMade puzzle = 1 + (length . guesses) puzzle

checkGuess :: String -> String -> [CharStatus]
checkGuess answer guess =
  let check = zip (isGreen answer guess) (isYellow answer guess)
   in map
        ( \(g, y) ->
            case (g, y) of
              (True, _) -> InRightPlace
              (_, True) -> InWord
              (_, _) -> NotInWord
        )
        check

guessWord :: Puzzle -> String -> Puzzle
guessWord (Puzzle p guesses) guess =
  let newGuess = Guess (zip guess (checkGuess p guess))
   in Puzzle p (newGuess : guesses)

puzzleWin :: Puzzle -> Bool
puzzleWin puzzle =
  case guesses puzzle of
    [] -> False
    (Guess g) : _ -> all ((== InRightPlace) . snd) g

mkPuzzle :: String -> Puzzle
mkPuzzle s = Puzzle (map toUpper s) []

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

newPuzzle :: IO Puzzle
newPuzzle = do
  newWord <- randomWord puzzleWords
  return $ mkPuzzle newWord