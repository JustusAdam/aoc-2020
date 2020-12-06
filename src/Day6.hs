#!/usr/bin/env stack
-- stack --resolver lts-16.15 script --package containers

{-# LANGUAGE TupleSections, LambdaCase, FlexibleInstances #-}

import Data.List (break, find, span)
import Data.Maybe (fromJust)
import System.Environment
import Control.Arrow ( (***) )
import Debug.Trace
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Data.Char

groups :: [String] -> [[String]]
groups [] = []
groups x = g: case xs of [] -> []; (_:gs) -> groups gs
  where
    (g,xs) = break (== "") x

main = do
    [f] <- getArgs
    putStrLn "Started"
    inp <- groups . lines <$> readFile f
    let res = sum [ Set.size $ foldr1 Set.intersection $ map Set.fromList checked
                  | checked <- inp
                  ]
    print res
