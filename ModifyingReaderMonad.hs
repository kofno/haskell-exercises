import Control.Monad.Reader

calculateContentLen :: Reader String Int
calculateContentLen = do
  content <- ask
  return (length content)

calculateModifiedContentLen :: Reader String Int
calculateModifiedContentLen = local ("Prefix " ++) calculateContentLen

main :: IO ()
main = do
  let s = "12345"
  let modifiedLen = runReader calculateModifiedContentLen s
  let len = runReader calculateContentLen s
  putStrLn $ "Modified 's' length: " ++ (show modifiedLen)
  putStrLn $ "Original 's' length: " ++ (show len)
