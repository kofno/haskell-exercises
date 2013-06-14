import System.Random
import Data.List
import Control.Monad.Reader

type Solution = [Char]
type Guesses = [Char]

type  Hangman = (Solution, Guesses)

type Message = String
data Result = Loss Message
            | Win Message
            | GameOn
            deriving (Show, Eq, Ord)

createGame :: Solution -> Hangman
createGame cs = (cs,"")

guess :: Hangman -> Char -> Hangman
guess game@(solution, guesses) c
  | alreadyGuessed = game
  | otherwise      = (solution, c:guesses)
  where alreadyGuessed = c `elem` guesses

missedGuesses :: Hangman -> Int
missedGuesses (solution, guesses) = length [ x | x <- guesses
                                               , x `notElem` solution ]
check :: Hangman -> Result
check game@(solution, guesses)
  | loser         = Loss "Hanged!"
  | completedWord = Win "Phew! You won"
  | otherwise     = GameOn
  where completedWord = solution == [ x | x <- solution, x `elem` guesses || x == ' '] 
        loser         = missedGuesses game > 5

main :: IO ()
main = do
  solution <- readSolution
  gameLoop $ createGame solution

readSolution :: IO String
readSolution = do
  solutions  <- readFile "hangmanSolutions.txt"
  i          <- randomRIO (0, length' solutions - 1)
  return (lines solutions !! i)
  where length' solutions = length . lines $ solutions

gameLoop :: Hangman -> IO ()
gameLoop game = do
  putScreen game
  letter <- getChar
  case check $ newGame letter of Loss cs -> putEndGame cs
                                 Win cs  -> putEndGame cs
                                 GameOn  -> gameLoop $ newGame letter
    where newGame c = guess game c

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

putScreen :: Hangman -> IO ()
putScreen game = do
  clearScreen
  putStrLn ""
  putStrLn $ maskSolution game
  putStrLn $ unusedLetters game
  putStrLn $ "Number of misses: " ++ show (missedGuesses game)
  putStr "Enter a guess: "

putEndGame :: String -> IO ()
putEndGame msg = do
  clearScreen
  putStrLn ""
  putStrLn msg

maskSolution :: Hangman -> String
maskSolution (solution, guesses) = intersperse ' ' $ map (mask guesses) solution
  where mask cs c = if c `elem` cs || c == ' ' then c else '_'

unusedLetters :: Hangman -> String
unusedLetters (_, guesses) = intersperse ' ' $ map (mask guesses) ['a'..'z']
  where mask cs c = if c `elem` cs then ' ' else c
