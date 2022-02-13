module Main where

import System.IO
  ( BufferMode (NoBuffering),
    hSetBuffering,
    stdout,
  )
import Puzzle (newPuzzle)
import Wordle (gameLoop, introString)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn introString
  puzzle <- newPuzzle
  gameLoop puzzle
