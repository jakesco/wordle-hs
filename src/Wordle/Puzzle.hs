module Puzzle where

import Data.Char (isAlpha, toUpper)
import Data.List (elemIndices, insert)
import Dictionary (WordList (..), alphabet, puzzleWords)
import System.Console.Pretty
  ( Color (..),
    Pretty,
    Style (..),
    bgColor,
    color,
    style,
  )
import System.Random (randomRIO)

type Answer = String

data CharStatus = NotChecked | NotInWord | InWord | InRightPlace
  deriving (Eq, Ord, Show)

data Letter = Letter
  { char :: Char,
    status :: CharStatus
  }
  deriving (Eq)

type Guess = [Letter]

instance Show Letter where
  show (Letter c s) = getColor s $ pad c

data Puzzle = Puzzle
  { answer :: Answer,
    guesses :: [Guess]
  }
  deriving (Eq)

instance Show Puzzle where
  show (Puzzle p guesses) =
    case guesses of
      [] -> concatMap pad p
      (g : _) -> concatMap show g

pad :: Char -> String
pad c
  | isAlpha c = [' ', toUpper c, ' ']
  | otherwise = [c]

getColor :: Pretty a => CharStatus -> a -> a
getColor cs =
  case cs of
    InRightPlace -> bgColor Green
    InWord -> bgColor Yellow
    NotInWord -> style Bold . color Black . bgColor White
    _ -> id

searchGuess :: Guess -> Char -> Letter
searchGuess gs c =
  Letter
    c
    ( foldr
        (max . status . (gs !!))
        NotChecked
        $ elemIndices c (map char gs)
    )

showGuessed :: Puzzle -> String
showGuessed (Puzzle _ gs) =
  concatMap (show . searchGuess (concat gs)) alphabet

guessesMade :: Puzzle -> Int
guessesMade puzzle = 1 + (length . guesses) puzzle

charInAnswer :: Answer -> Char -> Bool
charInAnswer answer = (`elem` answer)

findGreen :: Answer -> String -> Guess
findGreen answer guess =
  let greens = zip guess $ zipWith (==) answer guess
   in map
        ( \(c, s) ->
            if s
              then Letter c InRightPlace
              else Letter c NotChecked
        )
        greens

countOccurances :: Eq a => a -> [a] -> Int
countOccurances c = length . filter (== c)

findYellow :: Answer -> String -> Guess -> Guess
findYellow _ _ [] = []
findYellow answer known (g : gs) =
  case g of
    (Letter c NotChecked) ->
      if (countOccurances c answer > countOccurances c known)
        && charInAnswer answer c
        then Letter c InWord : findYellow answer (c : known) gs
        else Letter c NotInWord : findYellow answer known gs
    _ -> g : findYellow answer known gs

checkGuess :: String -> String -> Guess
checkGuess answer guess =
  let greens = findGreen answer guess
      known = char <$> filter (\(Letter _ s) -> s == InRightPlace) greens
   in findYellow answer known greens

guessWord :: Puzzle -> String -> Puzzle
guessWord (Puzzle p guesses) guess =
  let newGuess = checkGuess p guess
   in Puzzle p (newGuess : guesses)

puzzleWin :: Puzzle -> Bool
puzzleWin puzzle =
  case guesses puzzle of
    [] -> False
    g : _ -> all ((== InRightPlace) . status) g

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
