module Puzzle where

import Data.Char (toUpper)
import Dictionary (WordList (..), puzzleWords)
import System.Console.Pretty (Color (..), Pretty, bgColor)
import System.Random (randomRIO)

type Answer = String

data CharStatus = InRightPlace | InWord | NotInWord | NotChecked
  deriving (Eq, Show)

newtype Guess = Guess [(Char, CharStatus)]
  deriving (Eq)

instance Show Guess where
  show (Guess g) = foldr (\g -> (++) (getColor (snd g) $ pad (fst g))) [] g

pad :: Char -> String
pad c = [' ', toUpper c, ' ']

getColor :: Pretty a => CharStatus -> a -> a
getColor cs =
  case cs of
    InRightPlace -> bgColor Green
    InWord -> bgColor Yellow
    NotInWord -> bgColor Red
    _ -> bgColor Cyan

data Puzzle = Puzzle
  { answer :: Answer,
    guesses :: [Guess]
  }
  deriving (Eq)

instance Show Puzzle where
  show (Puzzle p guesses) =
    case guesses of
      [] -> concatMap pad p
      (g : _) -> show g

guessesMade :: Puzzle -> Int
guessesMade puzzle = 1 + (length . guesses) puzzle

charInAnswer :: Answer -> Char -> Bool
charInAnswer answer = (`elem` answer)

findGreen :: Answer -> String -> [(Char, CharStatus)]
findGreen answer guess = zip guess $ map (\s -> if s then InRightPlace else NotChecked) $ zipWith (==) answer guess

countOccurances :: Eq a => a -> [a] -> Int
countOccurances c = length . filter (== c)

findYellow :: Answer -> String -> [(Char, CharStatus)] -> [(Char, CharStatus)]
findYellow _ _ [] = []
findYellow answer known (g : gs) =
  case g of
    (c, NotChecked) ->
      if (countOccurances c answer > countOccurances c known) && charInAnswer answer c
        then (c, InWord) : findYellow answer (c : known) gs
        else (c, NotInWord) : findYellow answer known gs
    (_, _) -> g : findYellow answer known gs

checkGuess :: String -> String -> Guess
checkGuess answer guess =
  let greens = findGreen answer guess
      known = fst <$> filter (\(_, s) -> s == InRightPlace) greens
   in Guess (findYellow answer known greens)

guessWord :: Puzzle -> String -> Puzzle
guessWord (Puzzle p guesses) guess =
  let newGuess = checkGuess p guess
   in Puzzle p (newGuess : guesses)

puzzleWin :: Puzzle -> Bool
puzzleWin puzzle =
  case guesses puzzle of
    [] -> False
    (Guess g) : _ -> all ((== InRightPlace) . snd) g

mkPuzzle :: String -> Puzzle
mkPuzzle s = Puzzle (map toUpper s) []

testPuzzle :: Puzzle
testPuzzle = mkPuzzle "hello"

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

newPuzzle :: IO Puzzle
newPuzzle = do
  newWord <- randomWord puzzleWords
  return $ mkPuzzle newWord