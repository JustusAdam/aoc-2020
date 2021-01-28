#!/usr/bin/env stack
-- stack --resolver lts-16.15 script --package containers --package vector --package extra

{-# LANGUAGE TupleSections, LambdaCase, FlexibleInstances, ViewPatterns #-}

import Data.List (find, span, tails, sort)
import Data.Maybe (fromJust, mapMaybe)
import System.Environment
import Control.Arrow (second, first, (***) )
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
import Data.Foldable
import Data.Tuple
import Data.List.Extra (split)

solve :: Int -> [Int] -> Int
solve target = uncurry (*) . minimumBy (compare `on` snd) . map (\i -> (i, i - target `rem` i))

comb :: (Int, Int) -> (Int, Int) -> (Int, Int)
comb (x_off, x) (y_off, y) = go (\ts -> let gcd = go id (ts - x_off) (ts - y_off) in traceShowId (gcd - ts, gcd)) x_off y_off
  where
    go callback = go0
      where
        go0 xn yn | xn == yn = callback xn
                  | xn < yn = go0 (xn + x) yn
                  | otherwise = go0 xn (yn + y)

solve2 :: [(Int, Int)] -> Int
solve2 = abs . uncurry ((-)) . foldr1 comb

main = do
    [t, busses] <- lines <$> getContents
    print $ solve2 (map ((\case (0, n) -> (n, n); other -> other ) . second read) $ filter ((/= "x") . snd) $ zip [0..] $ split (== ',') busses)
