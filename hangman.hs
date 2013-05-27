import System.Console.ANSI
import Data.List

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

check :: Hangman -> Result
check (solution, guesses)
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
maskSolution (solution, guesses) = intersperse ' ' $ map (mask guesses) solution
  where mask cs c = if c `elem` cs then c else '_'

unusedLetters :: Hangman -> String
unusedLetters (_, guesses) = intersperse ' ' $ map (mask guesses) ['a'..'z']
  where mask cs c = if c `elem` cs then ' ' else c
