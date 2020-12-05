#!/usr/bin/env stack
-- stack --resolver lts-16.15 script

{-# LANGUAGE TupleSections #-}

import Data.List (find, span)
import Data.Maybe (fromJust)
import System.Environment


data Policy = Policy Int Int Char

data Line = Line Policy String

instance Read Policy where
    readsPrec p s = do
        (i1, '-':r) <- readsPrec p s
        (i2, ' ':r2) <- readsPrec p r
        let (c:cs) = r2
        pure (Policy i1 i2 c, cs)

instance Read Line where
    readsPrec p s = do
        (p, ':':' ':s) <- readsPrec p s
        let (str, other) = span (/= '\n') s
        pure (Line p str, drop 1 other)

solve :: [Line] -> Int
solve = length . filter valid
  where
    valid (Line (Policy from to c) s) =
        length (filter (== Just c) $ [getAt from s, getAt to s]) == 1
      where
        getAt 1 (x:_) = Just x
        getAt i (_:xs) = getAt (i-1) xs
        getAt _ [] = Nothing

main = do
    [f] <- getArgs
    inp <- map read . lines <$> readFile f
    print $ solve inp
