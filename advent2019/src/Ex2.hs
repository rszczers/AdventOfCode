{-# LANGUAGE
    TemplateHaskell
  , TypeApplications
  , OverloadedStrings
#-}

-- Don't panic; it bloated because I wanted to play with monad transformers
module Ex2 (solution) where

import Control.Lens
import Data.Text 
import Data.List as L (find)
import Control.Monad.State
import Data.Vector 
import Control.Monad.Trans.Maybe
import qualified Data.Vector as V

data Operation a = Lit a
                 | Add a a a
                 | Mul a a a
                 | Exit
                 deriving (Show)

data Computation = Computation
  { _counter :: Int
  , _memory :: Vector Int
  } deriving (Show)
makeLenses ''Computation

getOperation :: MaybeT (State Computation) (Operation Int)
getOperation = do
  comp <- get
  let (v, c) = (comp ^. memory, comp ^. counter)
  MaybeT . return $
    case v !? c of
      Just 1 -> do px' <- v !? (c + 1)
                   x' <- v !? px'
                   py' <- v !? (c + 2)
                   y' <- v !? py'
                   p  <- v !? (c + 3)
                   return $ Add x' y' p
      Just 2 -> do px' <- v !? (c + 1)
                   x' <- v !? px'
                   py' <- v !? (c + 2)
                   y' <- v !? py'
                   p  <- v !? (c + 3)
                   return $ Mul x' y' p
      Just 99 -> Just Exit
      _ -> Nothing
     
updateMemo :: Int -- ^ Result to be updated
           -> Int -- ^ Index
           -> Computation
           -> Computation
updateMemo r i comp = comp & (memory %~ (// [(i, r)]))

evalOp :: Operation Int -> MaybeT (State Computation) (Int, Int)
evalOp op = do
  comp <- get
  MaybeT $ 
    case op of
      Add x y p -> do put $ comp & (counter +~ 4)
                      return $ Just (x + y, p)
      Mul x y p -> do put $ comp & (counter +~ 4)
                      return $ Just (x * y, p)
      _ -> return Nothing

compute :: MaybeT (State Computation) Int
compute = do 
  comp <- get
  let mNextOp = evalState (runMaybeT getOperation) comp
  case mNextOp of
    Nothing -> MaybeT $ return Nothing
    Just Exit -> MaybeT . return . Just . V.head $ comp ^. memory 
    Just nextOp -> do
      let (mresp, comp') = runState (runMaybeT $ evalOp nextOp) comp
      put comp'
      case mresp of
        Just (res, p) -> do
             put $ updateMemo res p comp'
             compute
        _ -> error "Bad input"

solutionB :: IO ()
solutionB = do
  input <- pack <$> readFile "data/day2.dat"
  let d = V.fromList $ read @Int . unpack <$> splitOn "," input
      f = (\x y -> [(1, x), (2, y)]) <$> [0..99] <*> [0..99]
      c = (\z -> Computation 0 (d // z)) <$> f
      r = L.find (\(r, s) -> r == Just 19690720) $ runState (runMaybeT compute) <$> c
  print r

solutionA :: IO ()
solutionA = do
  input <- pack <$> readFile "data/day2.dat"
  let d = V.fromList $ read @Int . unpack <$> splitOn "," input
      c0 = Computation 0 (d // [(1,12), (2,2)])
      s = evalState (runMaybeT compute) c0
  print s

solution :: IO ()
solution = do
  putStrLn "Part I: "
  solutionA
  putStrLn "Part II:"
  solutionB
