#!/usr/bin/env stack
-- stack --resolver lts-16.15 script

{-# LANGUAGE TupleSections #-}

import Data.List (find)
import Data.Maybe (fromJust)
import System.Environment

u_uncurry f (a, b, c) = a `f` b `f` c

solve :: [Int] -> Int
solve = u_uncurry (*) . fromJust . find ((== 2020) . u_uncurry (+)) . perm3
  where
    perm3 (x:xs) = map (\(y, z) -> (x, y, z)) (perm xs) ++ perm3 xs
    perm3 [] = []
    perm (x:xs) = map (x,) xs ++ perm xs
    perm [] = []

main = do
    [f] <- getArgs
    inp <- map read . lines <$> readFile f
    print $ solve inp
