import Data.Monoid
import Control.Monad.State

isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Comapred gang size to 9.")

applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)
applyLog (x,log) f = let (y, newlog) = f x in (y, log `mappend` newlog)


type Food = String
type Price = Sum Int

addDrink :: Food -> (Food,Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _       = ("beer", Sum 30)

type Stack = [Int]

pop :: State Stack Int
pop = state $ \(x:xs) -> (x,xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((),a:xs)

stackManip :: State Stack Int
stackManip = do
  push 3
  a <- pop
  pop
