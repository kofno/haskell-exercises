import Control.Monad.Writer

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multiWithLog :: Writer [String] Int
multiWithLog = do
  a <- logNumber 3
  b <- logNumber 5
  tell ["Gonna multiply these two."]
  return (a*b)

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
  | b == 0    = do
      tell ["Finished with " ++ show a]
      return a    
  | otherwise = do
      tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
      gcd' b (a `mod` b)
