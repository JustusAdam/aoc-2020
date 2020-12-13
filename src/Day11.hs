#!/usr/bin/env stack
-- stack --resolver lts-16.15 script --package containers --package vector

{-# LANGUAGE TupleSections, LambdaCase, FlexibleInstances, ViewPatterns #-}

import Data.List (find, span, tails, sort)
import Data.Maybe (fromJust, mapMaybe)
import System.Environment
import Control.Arrow (second, (***) )
import Debug.Trace
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Data.Char
import Control.Monad
import Control.Applicative ((<|>))
import Control.Monad.ST.Strict
import Data.STRef.Strict
import Data.Function
import qualified Data.IntMap as IM
import Control.Category ((>>>))
import GHC.Arr hiding (indices)

type Field s = STArray s (Int, Int) Char

surrounds ((xLowerBound, yLowerBound),(xUpperBound, yUpperBound)) (x,y) =
    [ (a,b)
    | a <- [max xLowerBound (pred x) .. min xUpperBound (succ x)]
    , b <- [max yLowerBound (pred y)..min yUpperBound (succ y)]
    , not (a == x && b == y)
    ]

indices ((xLowerBound, yLowerBound), (xUpperBound, yUpperBound)) =
    [ (x, y)
    | x <- [xLowerBound..xUpperBound]
    , y <- [yLowerBound..yUpperBound]
    ]

diagonals = tail [(x *** y)| x <- ops, y <- ops]
  where ops = [id, pred, succ]

sumOccupied arr ind = do
    length . filter ('#'==) <$> traverse (readSTArray arr) ind

f1 from index state = do
    let b = boundsSTArray from
    occupied <- sumOccupied from (surrounds b index)
    state <- readSTArray from index
    pure $ case (state, occupied) of
        ('L',0) -> '#'
        ('#', n) | n >= 4 -> 'L'
        _ -> state

outOfBounds ((xLowerBound, yLowerBound), (xUpperBound, yUpperBound)) (x, y) =
    xLowerBound > x || xUpperBound < x
    || yLowerBound > y || yUpperBound < y

f2 from index state = do
    let b = boundsSTArray from
    let go f index | outOfBounds b index = pure 0
                   | otherwise = do
                         elem <- readSTArray from index
                         case elem of
                             'L' -> pure 0
                             '#' -> pure 1
                             _ -> go f (f index)

    occupied <- sum <$> traverse (\f -> go f (f index)) diagonals
    state <- readSTArray from index
    pure $ case (state, occupied) of
        ('L',0) -> '#'
        ('#', n) | n >= 5 -> 'L'
        _ -> state

step :: (Field s -> (Int, Int) -> Char -> ST s Char) -> Field s -> Field s -> ST s Bool
step f from to = do
    let b = boundsSTArray from
    changed <- newSTRef False
    forM_ (indices b) $ \index -> do
        state <- readSTArray from index
        newState <- f from index state
        modifySTRef changed (|| (state /= newState))
        writeSTArray to index newState
    readSTRef changed

toFixpoint :: [String] -> Int
toFixpoint states = runST $ do
   arr1 <- newSTArray ((0, 0), (pred $ length (head states), pred $ length states)) undefined
   arr2 <- newSTArray ((0, 0), (pred $ length (head states), pred $ length states)) undefined
   forM_ (zip [0..] states) $ \(i, l) ->
       forM_ (zip [0..] l) $ \(j, e) ->
       writeSTArray arr1 (j, i) e
   go arr1 arr2
 where
   go arr1 arr2 =
       step f2 arr1 arr2 >>= \case
       True -> go arr2 arr1
       False -> foldrElems' (\case '#' -> succ; _ -> id) 0 <$> freezeSTArray arr1

main = do
    inp <- lines <$> getContents
    print $ toFixpoint inp
