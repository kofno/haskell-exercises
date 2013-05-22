import Data.List

sortLines :: String -> String
sortLines = unlines . sort . lines

main = interact sort
