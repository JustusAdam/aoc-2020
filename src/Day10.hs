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
import qualified Data.Vector.Mutable as V
import Control.Monad.ST.Strict
import Data.STRef.Strict
import Data.Function
import qualified Data.IntMap as IM
import Control.Category ((>>>))

mkDist :: [Int] -> IM.IntMap Int
mkDist =
    sort
    >>> (0:) -- The outlet
    >>> tails
    >>> mapMaybe (\case (x:y:_) -> Just $ y - x
                        [_] -> Just 3 -- the device
                        _ -> Nothing)
    >>> map (,1)
    >>> IM.fromListWith (+)

permutes :: [Int] -> Int
permutes =
    sort
    >>> (0:)
    >>> go
    >>> (snd . head)
  where
    go [x] = [(x, 1), (x+3, 1)]
    go (x:xs) =
        let r = go xs in
        (x, sum $ map snd $ takeWhile ((<= x + 3) . fst) r):r

main = do
    inp <- map read . lines <$> getContents
    print $ permutes inp
