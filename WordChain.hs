module WordChain where

import Data.Set (Set, fromDistinctAscList, member)
import System.IO.Unsafe (unsafePerformIO)
import Data.Tree

type Path = [String]
    
goalPath :: String -> Path -> Bool
goalPath target ws = target == head ws

childPaths :: Path -> [Path]
childPaths (w:ws) =
    [s : w : ws | s <- oneStep w, s `notElem` ws]

initPath :: String -> Path
initPath w = [w]

wordChainPath a b =
    breadth_first_search (goalPath b) childPaths $ initPath a
                     
oneStep :: String -> [String]
oneStep = filter isValidWord . generateCandidates

isValidWord w = member w dict

generateCandidates :: String -> [String]
generateCandidates [] = []
generateCandidates (c:cs) = [ x : cs | x <- ['a'..'z'], x /= c]
                            ++ [ c : xs | xs <- generateCandidates cs]

stateTree :: (s -> [s]) -> s -> Tree s
stateTree children_of state =
    Node state [ stateTree children_of s | s <- children_of state]

-- helper so we can view part of the stateTree
prune :: Int -> Tree a -> Tree a
prune d (Node x cs)
    | d <= 0    = Node x []
    | otherwise = Node x [ prune (d - 1) c | c <- cs ]

bft :: Tree a -> [a]
bft t = bft_ [t]

bft_ :: [Tree a] -> [a]
bft_ trees = [ x | Node x _ <- trees ]
             ++ bft_ (concat $ map subForest trees)

breadth_first_search :: (s -> Bool) -- goal test
                     -> (s -> [s])  -- state generator
                     -> s           -- initial state
                     -> [s]         -- solutions out
breadth_first_search is_goal children_of =
    filter is_goal . bft . stateTree children_of

wordChain a b = breadth_first_search (\w -> w == b) oneStep a

dict = fromDistinctAscList
       $ lines
       $ unsafePerformIO (readFile "fours.txt")

