module HangmanM
( GameState
, Result(..)
, initialState
, guess
) where

import Control.Monad.State

type Solution  = String
type Guesses   = String
type Misses    = Int
type GameState = (Solution, Guesses, Misses)
type Hangman   = State GameState

data Result = AlreadyGuessed
            | Hit
            | Miss
            | Win
            | Loss
            deriving (Show, Eq, Ord)

initialState :: String -> GameState
initialState s = (s, "", 0)

guess :: Char -> Hangman Result
guess c = do
  result <- check c
  case result of
       Miss -> do
         (solution, guesses, misses) <- get
         put (solution, c:guesses, misses + 1)
         failed <- checkForFailure
         if failed
            then return Loss
            else return result

       Hit -> do
         (solution, guesses, misses) <- get
         put (solution, c:guesses, misses)
         solved <- checkForSolved
         if solved
            then return Win
            else return result

       _ -> return result
   where checkForSolved = do
           (solution, guesses, _) <- get
           return (solution == [ x | x <- solution, x `elem` guesses || x == ' ' ])

         checkForFailure = do
           (_, _, misses) <- get
           return (misses >= 5)

check :: Char -> Hangman Result
check c = do
  (_, guesses, _) <- get
  if c `elem` guesses
     then return AlreadyGuessed
     else hitOrMiss c

hitOrMiss :: Char -> Hangman Result
hitOrMiss c = do
  (solution, _, _) <- get
  if c `elem` solution
     then return Hit
     else return Miss
