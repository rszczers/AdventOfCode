module Day1 where

import Data.Map (fromListWith, toList)
import Control.Monad.State.Lazy

type Freq = Int
type Change = Int
type Quantity = Int

parseFuckers :: String -> Change
parseFuckers (s:r)
  | s == '+'  = p
  | s == '-'  = negate p
  | otherwise = error "Input error"
  where p = read r :: Change

solutionA = do
    input <- readFile "data/input1"
    putStrLn $ show $ foldr (+) 0 $ parseFuckers <$> lines input 

solutionB = undefined
    
