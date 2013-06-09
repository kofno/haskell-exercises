import Control.Monad.Reader

printReaderContent :: ReaderT String IO ()
printReaderContent = do
  content <- ask
  liftIO $ putStrLn ("The Reader Content: " ++ content)

main :: IO ()
main = do
  runReaderT printReaderContent "Some content"
