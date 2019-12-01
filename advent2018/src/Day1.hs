module Day1 (solutionA, solutionB) where

import qualified Data.Set as S

type Freq = Int
type Change = Int
type Quantity = Int

parse :: String -> Change
parse (s:r)
  | s == '+'  = p
  | s == '-'  = negate p
  | otherwise = error "Input error"
  where p = read r :: Change

findRepetition :: [Freq] -> Maybe Freq
findRepetition [] = Nothing
findRepetition k = go S.empty k
  where
    go _ [] = Nothing
    go s (x:xs)
      | x `S.member` s = Just x
      | otherwise      = go (x `S.insert` s) xs

solutionA = do
    readFile "data/input1" >>=
      putStrLn . show . frequency 
    where
      frequency = \x -> (sum . (fmap parse) . lines) x

solutionB = do
    readFile "data/input1" >>=
      putStrLn . show . findRepetition . frequencies 
    where
      frequencies = \x -> ((scanl (+) 0) . map parse . cycle . lines) x 
