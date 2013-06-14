import HangmanM
import System.Random
import Control.Monad.State
import Data.List (intersperse)

main :: IO ()
main = do
  solution <- readSolution
  clearScreen
  nextGuess . initialState $ solution

readSolution :: IO String
readSolution = do
  solutions <- readFile "hangmanSolutions.txt"
  i         <- randomRIO (0, length' solutions - 1)
  return (lines solutions !! i)
  where length' solutions = length . lines $ solutions

nextGuess :: GameState -> IO ()
nextGuess game = do
  putGame game
  letter <- getChar
  putResult $ runState (guess letter) game

putResult :: (Result, GameState) -> IO ()

putResult (Win, _) = do
  clearScreen
  putStrLn "Winner!"

putResult (Loss, (solution, _, _)) = do
  clearScreen
  putStrLn $ "Loser! The answer was '" ++ solution ++ "'"

putResult (AlreadyGuessed, game) = do
  clearScreen
  putStrLn "You already guessed that!"
  nextGuess game

putResult (Hit, game) = do
  clearScreen
  putStrLn "Nice guess!"
  nextGuess game

putResult (Miss, game) = do
  clearScreen
  putStrLn "Bzzzzt!"
  nextGuess game

putGame :: GameState -> IO ()
putGame (solution, guesses, misses) = do
  putStrLn ""
  putStrLn maskSolution
  putStrLn remainingLetters
  putStrLn $ "Number of misses: " ++ show misses
  putStr "Enter a guess: "
  where maskSolution = intersperse ' ' $ map (mask elem guesses) solution
        remainingLetters = intersperse ' ' $ map (mask notElem guesses) ['a'..'z']
        mask f cs c = if f c cs || c == ' ' then c else '_'

clearScreen :: IO ()
clearScreen = do
  putStr "\ESC[2J"
  putStrLn ""
