module Ex1 where

parseFuckers :: String -> Int
parseFuckers (s:r)
  | s == '+'  = p
  | s == '-'  = negate p
  | otherwise = error "Input error"
  where p = read r :: Int

solution = do
    input <- readFile "data/input1"
    putStrLn $ show $ foldr (+) 0 $ parseFuckers <$> lines input 
