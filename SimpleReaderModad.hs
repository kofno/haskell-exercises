import qualified Data.Map as M
import Data.Maybe
import Control.Monad
import Control.Monad.Reader

type Bindings = M.Map String Int

isCountCorrect :: Bindings -> Bool
isCountCorrect bindings = runReader calcIsCountCorrect bindings

calcIsCountCorrect :: Reader Bindings Bool
calcIsCountCorrect = do
  count    <- asks $ lookupVar "count"
  bindings <- ask
  return (count == (M.size bindings))

lookupVar :: String -> Bindings -> Int
lookupVar name bindings = fromJust (M.lookup name bindings)

sampleBindings :: Bindings
sampleBindings = M.fromList [("count",3), ("1", 1), ("b", 2)]

main :: IO ()
main = do
  putStr   $ "Count is correct for bindings " ++ (show sampleBindings) ++ ": "
  putStrLn $ show (isCountCorrect sampleBindings)
