-- The orginal word chain kata was missing it's word file, so I wrote
-- this to convert an ispell dictionary that I had laying around to a
-- word list w/ just four letter words (all lowercase).

module Main where

import Data.Char
import qualified Text.PortableLines as PL

main = do content <- getContents
          putStrLn $ fourLetterWords content 

fourLetterWords :: String -> String
fourLetterWords s = unlines [ x | x <- PL.lines s, length x == 4, all isLower x]
