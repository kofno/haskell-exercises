import System.Console.ANSI
import Data.List

type Solution = [Char]
type Guesses = [Char]

data Hangman = Hangman Solution Guesses
        deriving (Show)

type Message = String
data Result = Loss Message
            | Win Message
            | GameOn
            deriving (Show, Eq, Ord)

createGame :: Solution -> Hangman
createGame cs = Hangman cs ""

guess :: Hangman -> Char -> Hangman
guess game@(Hangman solution guesses) c
  | alreadyGuessed = game
  | otherwise      = Hangman solution (c:guesses)
  where alreadyGuessed = c `elem` guesses

check :: Hangman -> Result
check (Hangman solution guesses)
  | loser         = Loss "Hanged!"
  | completedWord = Win "Phew! You won"
  | otherwise     = GameOn
  where completedWord = solution == [ x | x <- solution, x `elem` guesses] 
        loser         = length [ x | x <- guesses, x `notElem` solution] > 5

main :: IO ()
main = gameLoop $ createGame "solution"

gameLoop :: Hangman -> IO ()
gameLoop game = do
  clearScreen
  putStrLn ""
  putStrLn $ maskSolution game
  putStrLn $ unusedLetters game
  putStr "Enter a guess: "
  letter <- getChar
  case check $ newGame letter of Loss cs -> do
                                   clearScreen
                                   putStrLn ""
                                   putStrLn cs
                                 Win cs  -> do
                                   clearScreen
                                   putStrLn ""
                                   putStrLn cs
                                 GameOn  -> gameLoop $ newGame letter
    where newGame c = guess game c

maskSolution :: Hangman -> String
maskSolution (Hangman solution guesses) = intersperse ' ' $ map (mask guesses) solution
  where mask cs c = if c `elem` cs then c else '_'

unusedLetters :: Hangman -> String
unusedLetters (Hangman _ guesses) = intersperse ' ' $ map (mask guesses) ['a'..'z']
  where mask cs c = if c `elem` cs then ' ' else c
