{-# LANGUAGE
    TypeApplications
#-}

module Ex1 (solution, solution') where

import Control.Lens

fuel :: Float -> Float
fuel = flip (-) 2 . fromIntegral . floor @Float . (/ 3) 

solution :: IO ()
solution = do
  i <- fmap (read @Float) <$> (lines <$> readFile "data/day1.dat")
  let j = tail . takeWhile (> 0) . iterate fuel <$> i
  putStrLn $ "Part I: \t" ++ show (sum $ fuel <$> i)
  putStrLn $ "Part II: \t" ++ show (sum $ sum <$> j)

-- | With lens
solution' :: IO ()
solution' = do
  i <- readFile "data/day1.dat"
  print $ i ^.. worded
