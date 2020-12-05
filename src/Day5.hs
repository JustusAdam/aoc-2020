#!/usr/bin/env stack
-- stack --resolver lts-16.15 script --package containers

{-# LANGUAGE TupleSections, LambdaCase, FlexibleInstances #-}

import Data.List (find, span)
import Data.Maybe (fromJust)
import System.Environment
import Control.Arrow ( (***) )
import Debug.Trace
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Data.Char

offsetInto :: [Bool] -> Int -> Int
offsetInto [] i = i
offsetInto (x:xs) i = (if x then (+h) else id) $ offsetInto xs h
  where h = i `div` 2

decodePass :: String -> (Int, Int)
decodePass s = (offsetInto (map (== 'B') rowS) 128 - 1, offsetInto (map (== 'R') colS) 8 - 1)
  where
    (rowS, colS) = splitAt 7 s

seatId :: Int -> Int -> Int
seatId row col = row * 8 + col

main = do
    [f] <- getArgs
    inp <- Set.fromList . map (uncurry seatId . decodePass ) . lines <$> readFile f
    let all = Set.fromList [seatId row col | row <- [0..127], col <- [0..7]]
        candidates = Set.difference all inp
    print [x | x <- Set.toList candidates, Set.member (x+1) inp, Set.member (x-1) inp]
