module Day2 where

import Data.List
import Data.List.Unique (occurrences)

dhamming:: Eq a => [a] -> [a] -> Int
dhamming s1 s2 = length (filter id (zipWith (/=) s1 s2))

epsilon :: [String] -> (String, String)
epsilon x = head [(s1, s2) | s1 <- x,
                             s2 <- x,
                             s1 /= s2,
                             dhamming s1 s2 == 1]

common :: (String, String) -> String
common (a, b) = (map snd . filter (\(x, y) -> x == True) .
                  zip (zipWith (==) a b)) a

solutionA = do
    input <- readFile "data/input2"
    (putStrLn . show . hash) input
    where
      hash = product .
        map fst .
        occurrences .
        concat .
        map (map fst . filter (\(x, y) -> x == 2 || x == 3) .
        occurrences) . lines

solutionB = do
    input <- readFile "data/input2"
    (putStrLn . show . common . epsilon . lines) input
