{-# LANGUAGE OverloadedStrings #-}
module Day3 where

import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text
import Text.Parsec.Combinator 

data Claim = Claim { claimId :: Int
                   , coordx  :: Int
                   , coordy  :: Int
                   , width   :: Int
                   , height  :: Int }
                   deriving (Show)

parseClaim :: Parser Claim
parseClaim = do
    char '#'
    c1 <- many1 digit 
    char ' '
    char '@'
    char ' '
    c2 <- many1 digit
    char ','
    c3 <- many1 digit
    char ':'
    char ' '
    c4 <- many1 digit
    char 'x'
    c5 <- many1 digit
    return $ Claim (read c1) (read c2) (read c3) (read c4) (read c5)

isOverlaping :: Claim -> Claim -> Bool
isOverlaping a b = undefined

solutionA = do
  input <- readFile "data/input3"
  let claims = (sequence . map (parse parseClaim "") .
                 T.lines .
                 T.pack) input
  (putStr . show) claims

solutionB = undefined

