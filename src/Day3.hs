#!/usr/bin/env stack
-- stack --resolver lts-16.15 script

{-# LANGUAGE TupleSections, LambdaCase, FlexibleInstances #-}

import Data.List (find, span)
import Data.Maybe (fromJust)
import System.Environment
import Control.Arrow ( (***) )
import Debug.Trace

type M = Int -> Int -> Bool

mkMap :: [String] -> M
mkMap s = \x y -> (fs !! y) $ x `mod` i
  where
    fs = map toF  s
    i = length $ head  s
    toF (x:xs) = \case
        0 -> x == '#'
        n -> toF xs (n-1)
    toF [] = error "impossible"

solve :: (Int, Int) -> Int -> M -> Int
solve (right, down) threshold f = length $ filter (uncurry f) $ takeWhile ((< threshold) . snd) $ iterate ((+right) *** (+down)) (0,0)

main = do
    [f] <- getArgs
    inp <- lines <$> readFile f
    let map = mkMap inp
        directions = [(1, 1), (3, 1), (5, 1), (7,1), (1, 2)]
    print $ product [ solve slope (length inp) map | slope <- directions ]
