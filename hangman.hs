import System.Console.ANSI
import Data.List

type Solution = String
type Hits = String
type Misses = String

data Hangman = Hangman Solution Hits Misses
        deriving (Show)

type Message = String
data Result = Loss Message
            | Win Message
            | GameOn
            deriving (Show, Eq, Ord)

createGame :: Solution -> Hangman
createGame cs = Hangman cs "" ""

guess :: Hangman -> Char -> Hangman
guess game@(Hangman solution hits misses) c
  | alreadyGuessed = game
  | isHit          = Hangman solution (c:hits) misses
  | otherwise      = Hangman solution hits (c:misses)
  where isHit = c `elem` solution
        alreadyGuessed = c `elem` hits || c `elem` misses 

check :: Hangman -> Result
check (Hangman cs hs ms)
  | length ms > 5 = Loss "Hanged!"
  | completedWord = Win "Phew! You won"
  | otherwise     = GameOn
  where completedWord = cs == [ x | x <- cs, x `elem` hs] 

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
maskSolution (Hangman cs hits _) = intersperse ' ' $ map (mask hits) cs
  where mask cs c = if c `elem` cs then c else '_'

unusedLetters :: Hangman -> String
unusedLetters (Hangman _ _ misses) = intersperse ' ' $ map (mask misses) ['a'..'z']
  where mask cs c = if c `elem` cs then ' ' else c
